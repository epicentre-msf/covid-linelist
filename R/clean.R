#' Implement linelist cleaning routines
#'
#' @param dat Linelist data.frame
#' @param path_dictionaries Path to directory with recoding dictionaries
#' @param path_corrections_dates Path to directory with date corrections
#' @param date_vars vector of date variables
#' @param dict_factors Dictionary of allowed values for all factor variables
#' @param dict_countries Dictionary of ISO3 country codes
#' @param dict_numeric_correct Dictionary of corrections to numeric variables
#' @param dict_factors_correct Dictionary of corrections to categorical variables
#' @param dict_countries_correct Dictionary of corrections to ISO3 country variables
#' @param write_checks Logical indicating whether check files should be written
#'
#' @return
#' Cleaned linelist
#' 
clean_linelist <- function(dat,
                           path_dictionaries,
                           path_corrections_dates,
                           vars_date,
                           vars_numeric,
                           dict_factors,
                           dict_countries,
                           corr_numeric,
                           dict_factors_correct,
                           dict_countries_correct,
                           write_checks) {
  
  ## requires
  library(dplyr, warn.conflicts = FALSE)
  library(tidyr, warn.conflicts = FALSE)
  library(glue, warn.conflicts = FALSE)
  library(llutils, warn.conflicts = FALSE)
  library(matchmaker, warn.conflicts = FALSE)
  library(dbc, warn.conflicts = FALSE)
  source("R/clean.R")
  source("R/utilities.R")

  ## if running manually
  if (FALSE) {
    dat <- ll_import
    # dat <- ll_cleaned
    write_checks <- FALSE
  }
  
  #### Create temporary IDs if otherwise missing -------------------------------
  dat_prep_id <- dat %>% 
    group_by(site) %>% 
    mutate(
      temp_MSF_N_Patient = paste0("TEMPID_", formatC(1:n(), width = 3, flag = "0")),
      temp_patient_id = paste(site, format_text(temp_MSF_N_Patient), sep = "_")
    ) %>% 
    ungroup() %>% 
    mutate(
      temp_update_ids = is.na(MSF_N_Patient),
      MSF_N_Patient = ifelse(temp_update_ids, temp_MSF_N_Patient, MSF_N_Patient),
      patient_id = ifelse(temp_update_ids, temp_patient_id, patient_id)
    ) %>% 
    select(-temp_update_ids, -temp_MSF_N_Patient, -temp_patient_id)
  
  #### Clean numeric variables -------------------------------------------------
  
  ## Recalculate Comcond_present
  comcond_n <- dat_prep_id %>% 
    select(starts_with("Comcond"), -Comcond_present) %>% 
    mutate(Comcond_other = ifelse(!is.na(Comcond_other), "Yes", NA_character_)) %>% 
    apply(1, function(x) as.character(sum(tolower(x) %in% c('yes', 'oui', 'si', 'sim') & !is.na(x), na.rm = TRUE)))
  
  comcond_missing <- dat_prep_id %>% 
    select(starts_with("Comcond"), -Comcond_present) %>% 
    apply(1, function(x) all(tolower(x) %in% c('unknown', 'inconnu', 'no conocido', 'desconhecido') | is.na(x)))
  
  comcond_n[comcond_missing] <- NA_character_
  
  comcond_bad <- dat_prep_id$Comcond_present[!(is.na(dat_prep_id$Comcond_present) & is.na(comcond_n) | dat_prep_id$Comcond_present == comcond_n)]
  
  if (length(comcond_bad) > 0) {
    dat_prep_id$Comcond_present <- comcond_n
  }
  
  ## Clean all other numeric variables
  dat_numeric_prep <- dat_prep_id %>% 
    mutate(
      MSF_oxygen_saturation = gsub("\\%$", "", MSF_oxygen_saturation),
      MSF_oxygen_saturation = as.numeric(MSF_oxygen_saturation),
      MSF_oxygen_saturation = case_when(
        MSF_oxygen_saturation < 1 ~ MSF_oxygen_saturation * 100,
        TRUE ~ MSF_oxygen_saturation
      )
    )
  
  corr_numeric_update <- dbc::check_numeric(
    dat_numeric_prep,
    vars = vars_numeric,
    vars_id = "patient_id",
    queries = list(
      patinfo_ageonset < 0,
      outcome_contacts_followed < 0,
      patinfo_ageonset > 110,
      outcome_contacts_followed > 100,
      MSF_oxygen_saturation > 100
    ),
    dict_clean = corr_numeric,
    return_all = TRUE
  )
  
  if (any(corr_numeric_update$new %in% TRUE)) {
    
    # archive correction old file
    qxl::qxl(
      corr_numeric,
      file = file.path(path_corrections, "archive", glue::glue("corr_numeric_{time_stamp()}.xlsx"))
    )
    
    # update corr_numeric.xlsx
    qxl::qxl(
      corr_numeric_update,
      file = file.path(path_corrections, "corr_numeric.xlsx"),
      style1 = qxl::qstyle(halign = "left")
    )
    
    message(sum(corr_numeric_update$new %in% TRUE), " new ambiguous numeric value(s) written to corr_numeric.xlsx")
  }
  
  dat_numeric_clean <- dbc::clean_numeric(
    dat_numeric_prep,
    vars = vars_numeric,
    vars_id = "patient_id",
    dict_clean = corr_numeric
  )

  
  #### Clean date variables ----------------------------------------------------
  
  ## assemble dictionary for date variables
  corr_dates <- list.files(
    path_corrections_dates,
    pattern = "^dates_check_compiled.*\\.xlsx",
    full.names = TRUE
  ) %>% 
    purrr::map_dfr(readxl::read_xlsx, col_types = "text") %>%
    filter(!is.na(date_correct)) %>% 
    distinct(patient_id, variable, value, replacement = date_correct)
  
  corr_dates_update <- dbc::check_dates(
    dat_numeric_clean,
    vars = vars_date,
    vars_id = "patient_id",
    queries = list(
      .x > Sys.Date() + 1L,
      .x < as.Date("2020-02-01"),
      upload_date < report_date,
      as.numeric(outcome_date_of_outcome - MSF_date_consultation) < -5
    ),
    dict_clean = corr_dates,
    populate_na = TRUE
  ) %>% 
    filter(!is.na(value)) %>% 
    group_by(patient_id) %>% 
    arrange(date) %>% 
    ungroup()
  
  if (any(!is.na(corr_dates_update$query)) & write_checks) {
    
    corr_dates_update %>% 
      rename(date_correct = replacement, flag = query) %>% 
      mutate(comment = NA_character_) %>% 
      qxl::qxl(
        file = file.path(path_corrections_dates, glue("dates_check_compiled_{time_stamp()}.xlsx")),
        style1 = qxl::qstyle(halign = "left"),
        group = "patient_id",
        col_widths = c(date = 10, comment = 30)
      )
    
    message(sum(!is.na(corr_dates_update$query)), " new date problems written to file")
  }
  
  dat_dates_clean <- dbc::clean_dates(
    dat_numeric_clean,
    vars = vars_date,
    vars_id = "patient_id",
    dict_clean = corr_dates
  ) %>% 
    # recalculate after date corrections (if any)
    mutate(
      MSF_length_stay = as.numeric(outcome_date_of_outcome - MSF_date_consultation),
      MSF_delay_before_admission = as.numeric(MSF_date_consultation - patcourse_dateonset)
    )
  
  
  #### Clean categorical variables ---------------------------------------------
  dict_factors_long <- dict_factors %>% 
    mutate(english = values_en) %>% 
    pivot_longer(cols = starts_with("values"), names_to = "language") %>% 
    mutate(language = stringr::str_sub(language, -2)) %>% 
    arrange(language) %>% 
    mutate(value_std = string_std_simple(value)) %>% 
    select(value_std, value, variable, english, language)
  
  dat_factors_clean <- dat_dates_clean %>% 
    mutate(language = tolower(stringr::str_sub(linelist_lang, 1, 2))) %>% 
    mutate(language = recode(language, "po" = "pt")) %>% 
    mutate_at(unique(dict_factors$variable), string_std_simple) %>% 
    match_df_vec(dictionary = dict_factors_long, group = "language") %>% 
    match_df_vec(dictionary = dict_factors_correct, group = "language")
  
  
  # Check for remaining non-valid values
  check_categorical <- check_df_vec(dat_factors_clean,
                                    dict_factors_long,
                                    col_vals = 2,
                                    col_vars = 3,
                                    group = "language",
                                    always_allow_na = TRUE,
                                    return_allowed = TRUE,
                                    nchar_allowed = 120) %>% 
    as_tibble()

  
  
  if (nrow(check_categorical) > 0 & write_checks) {
    
    ## archive current dictionary
    file_out_archive <- glue::glue("dict_factors_correct_{time_stamp()}.xlsx")
    
    llutils::write_simple_xlsx(
      dict_factors_correct,
      file = file.path(path_dictionaries, "archive", file_out_archive)
    )
    
    ## update dictionary
    dict_factors_correct_write <- dict_factors_correct %>% 
      mutate(new = NA_character_)
    
    dict_factors_out <- check_categorical %>% 
      mutate(new = "Yes") %>% 
      bind_rows(dict_factors_correct_write, .) %>% 
      arrange(language, rev(new))
    
    llutils::write_simple_xlsx(
      dict_factors_out,
      file = file.path(path_dictionaries, glue("dict_factors_correct.xlsx"))
    )
    
    message(nrow(check_categorical), " new nonvalid values of categorical variables written to dict_factors_correct")
  }
  
  ### Convert to English
  dat_factors_english <- dat_factors_clean %>% 
    match_df_vec(dictionary = dict_factors_long,
                 from = "value",
                 to = "english",
                 by = "variable",
                 group = "language") %>% 
    select(-language)
  
  
  #### Clean ISO3 country variables --------------------------------------------
  vars_country <- c("report_country",
                    "patinfo_idadmin0",
                    "patinfo_resadmin0",
                    "patinfo_occuhcw_country",
                    "expo_case_location",
                    "expo_travel_country1",
                    "expo_travel_country2",
                    "expo_travel_country3")
  
  dat_countries_clean <- dat_factors_english %>%
    matchmaker::match_df(
      filter(dict_countries_correct, !is.na(replacement1)), ### bug when there is NA in replacement col
      from = "value1",
      to = "replacement1",
      by = "variable1"
    )
  
  check_countries <- queryr::query(
    dat_countries_clean, !.x %in% c(dict_countries$iso, NA_character_),
    cols_dotx = all_of(vars_country),
    cols_base = country,
    count = TRUE
  ) %>% 
    mutate(replacement1 = guess_countrycode(value1)) %>% 
    select(value1, replacement1, variable1, country, n)
  
  # write to dict
  if (nrow(check_countries) > 0 & write_checks) {
    
    ## archive current dictionary
    file_out_archive <- glue::glue("dict_countries_correct_{time_stamp()}.xlsx")
    
    llutils::write_simple_xlsx(
      dict_countries_correct,
      file = file.path(path_dictionaries, "archive", file_out_archive)
    )
    
    ## update dictionary
    dict_countries_correct_write <- dict_countries_correct %>% 
      mutate(new = NA_character_)
    
    dict_countries_out <- check_countries %>% 
      mutate(new = "Yes") %>% 
      bind_rows(dict_countries_correct_write, .) %>% 
      arrange(rev(new))
    
    llutils::write_simple_xlsx(
      dict_countries_out,
      file = file.path(path_dictionaries, glue("dict_countries_correct.xlsx"))
    )
    
    message(nrow(check_countries), " new nonvalid ISO3 country codes written to dict_countries_correct")
  }
  
  
  ### Calculate age in years
  dat_countries_clean$age_in_years <- llutils::age_in_years(
    dat_countries_clean$patinfo_ageonset,
    dat_countries_clean$patinfo_ageonsetunit
  )
  
  ### Update patcourse_admit and patcourse_presHCF
  types_admit <- c(
    "Admission in nonmedicalized structure (isolation)",
    "First hospitalisation",
    "First hospitalisation after a consultation",
    "Rehospitalisation"
  )
  
  dat_out <- dat_countries_clean %>% 
    mutate(
      patcourse_admit = case_when(
        MSF_visit_type %in% types_admit ~ "Yes",
        MSF_visit_type %in% c("First consultation") ~ "No",
        TRUE ~ patcourse_admit
      ),
      patcourse_presHCF = case_when(
        patcourse_admit == "Yes" & is.na(patcourse_presHCF) ~ MSF_date_consultation,
        TRUE ~ patcourse_presHCF
      ),
      # test
      outcome_patcourse_admit = case_when(
        !is.na(outcome_patcourse_admit) ~ outcome_patcourse_admit,
        patcourse_admit == "Yes" ~ "Yes"
      ),
    )
  
  ### return
  return(dat_out)
}





check_df_vec <- function(x,
                         dictionary,
                         col_vals = 1,
                         col_vars = 2,
                         group,
                         always_allow_na,
                         return_allowed = FALSE,
                         sep_allowed = "; ",
                         nchar_allowed = 60) {
  
  x_split <- split(x, x[[group]])
  
  groups_match <- names(x_split)
  dictionary_split <- purrr::map(groups_match, ~ filter(dictionary, !!ensym(group) == .x))
  names(dictionary_split) <- groups_match
  
  purrr::map2_dfr(
    x_split,
    dictionary_split,
    matchmaker::check_df,
    col_vals = col_vals,
    col_vars = col_vars,
    always_allow_na = always_allow_na,
    return_allowed = return_allowed,
    sep_allowed = sep_allowed,
    nchar_allowed = nchar_allowed,
    .id = "language"
  )
}



match_df_vec <- function(x, dictionary, from = 1, to = 2, by = 3, group) {
  
  x_split <- split(x, x[[group]])
  groups_match <- names(x_split)
  dictionary_split <- purrr::map(groups_match, ~ filter(dictionary, !!ensym(group) == .x))
  names(dictionary_split) <- groups_match
  
  purrr::map2_dfr(
    x_split,
    dictionary_split,
    matchmaker::match_df,
    from = from,
    to = to
  )
}

