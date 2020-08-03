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
                           date_vars,
                           dict_factors,
                           dict_countries,
                           dict_numeric_correct,
                           dict_factors_correct,
                           dict_countries_correct,
                           write_checks) {

  ## requires
  library(dplyr, warn.conflicts = FALSE)
  library(tidyr, warn.conflicts = FALSE)
  library(glue, warn.conflicts = FALSE)
  library(repi, warn.conflicts = FALSE)
  library(matchmaker, warn.conflicts = FALSE)
  source("R/clean.R")
  source("R/utilities.R")

  ## if running manually
  if (FALSE) {
    dat <- ll_cleaned
    write_checks <- TRUE
  }
  
  #### Create temporary IDs if otherwise missing -------------------------------
  dat <- dat %>% 
    group_by(site) %>% 
    mutate(temp_MSF_N_Patient = paste0("TEMPID_", formatC(1:n(), width = 3, flag = "0")),
           temp_patient_id = paste(site, format_text(temp_MSF_N_Patient), sep = "_")) %>% 
    ungroup() %>% 
    mutate(temp_update_ids = is.na(MSF_N_Patient)) %>% 
    mutate(MSF_N_Patient = ifelse(temp_update_ids, temp_MSF_N_Patient, MSF_N_Patient),
           patient_id = ifelse(temp_update_ids, temp_patient_id, patient_id)) %>% 
    select(-temp_update_ids, -temp_MSF_N_Patient, -temp_patient_id)
  
  
  #### Clean numeric variables -------------------------------------------------
  
  ## Recalculate Comcond_present
  comcond_n <- dat %>% 
    select(starts_with("Comcond"), -Comcond_present) %>% 
    mutate(Comcond_other = ifelse(!is.na(Comcond_other), "Yes", NA_character_)) %>% 
    apply(1, function(x) as.character(sum(tolower(x) %in% c('yes', 'oui', 'si', 'sim') & !is.na(x), na.rm = TRUE)))
  
  comcond_missing <- dat %>% 
    select(starts_with("Comcond"), -Comcond_present) %>% 
    apply(1, function(x) all(tolower(x) %in% c('unknown', 'inconnu', 'no conocido', 'desconhecido') | is.na(x)))
  
  comcond_n[comcond_missing] <- NA_character_
  
  comcond_bad <- dat$Comcond_present[!(is.na(dat$Comcond_present) & is.na(comcond_n) | dat$Comcond_present == comcond_n)]
  
  if (length(comcond_bad) > 0) {
    dat$Comcond_present <- comcond_n
    message("The following values of Comcond_present will be re-calculated: ",
            paste(unique(comcond_bad), collapse = "; "))
  }
  
  
  ## prepare dictionary
  dict_numeric_prep <- dict_numeric_correct %>% 
    mutate_at(vars(patient_id, variable, value), as.character) %>% 
    mutate_at(vars(replacement), as.integer) %>% 
    mutate(replace = TRUE)
    
  
  ## gather numeric variables, convert to numeric, and apply dictionary
  dat_numeric <- dat %>%
    select(db_row,
           patient_id,
           patinfo_ageonset,
           MSF_delay_before_admission,
           MSF_length_stay,
           Comcond_present,
           outcome_contacts_followed) %>% 
    tidyr::gather(variable, value, -db_row, -patient_id) %>%
    mutate(value_numeric = suppressWarnings(as.integer(value))) %>% 
    left_join(dict_numeric_prep, by = c("patient_id", "variable", "value")) %>% 
    mutate(replace = ifelse(is.na(replace), FALSE, replace)) %>% 
    mutate(value_numeric = ifelse(replace, replacement, value_numeric)) %>% 
    select(-replacement) %>% 
    mutate(flag = !is.na(value) & is.na(value_numeric) & !replace)
  
  ## check for non-missing values not converted to numeric
  if (any(dat_numeric$flag)) {

    ## archive current dictionary
    file_out_archive <- glue::glue("dict_numeric_correct_{time_stamp()}.xlsx")
    
    llutils::write_simple_xlsx(
      dict_numeric_correct,
      file = file.path(path_dictionaries, "archive", file_out_archive)
    )
    
    ## update dictionary
    check_numeric_append <- dat_numeric %>%
      filter(flag) %>% 
      select(patient_id, variable, value) %>% 
      mutate(replacement = NA_character_, new = "Yes")
    
    dict_numeric_correct_out <- dict_numeric_correct %>% 
      mutate(new = NA_character_) %>% 
      bind_rows(check_numeric_append)
    
    llutils::write_simple_xlsx(
      dict_numeric_correct_out,
      file = file.path(path_dictionaries, glue("dict_numeric_correct.xlsx"))
    )
    
    message(
      nrow(check_numeric_append),
      " new nonvalid values of numerical variables written to dict_numeric_correct"
    )
  }
  
  ## merge cleaned numeric columns back into dat_numeric
  dat_numeric_clean <- dat_numeric %>%
    select(-value, -replace, -flag) %>%
    tidyr::spread(variable, value_numeric) %>% 
    left_join_replace(x = dat, y = ., cols_match = c("db_row", "patient_id"))
  
  
  #### Clean date variables ----------------------------------------------------
  
  ## assemble dictionary for date variables
  check_files <- list.files(
    path_corrections_dates,
    pattern = "dates_check_compiled.*\\.xlsx",
    full.names = TRUE
  )
  
  dict_dates_full <- purrr::map_dfr(check_files, check_files_to_dict)
  
  if (nrow(dict_dates_full) == 0) dict_dates_full <- create_empty_dict_dates()
  # TODO: fix problem with prev-changed vars getting ignore in new queries
  dict_dates_ignore <- filter(dict_dates_full, ignore)
  
  dict_dates <- dict_dates_full %>% 
    filter(!ignore) %>% 
    select(-flag, -ignore) %>% 
    unique()
  
  ## gather date columns
  dat_date <- dat_numeric_clean %>%
    select(db_row, patient_id, all_of(date_vars)) %>%
    tidyr::gather(variable, value, -db_row, -patient_id) %>%
    arrange(db_row, patient_id) %>% 
    mutate(date = suppressWarnings(parse_excel_dates(value)),
           date = parse_other_dates(date),
           date = as.Date(date))
  
  dat_date <- dat_date %>% 
    repi::recode_conditional(dict = dict_dates, col_recode = "date", flag_recoded = TRUE) %>% 
    mutate(flag_ambiguous = ifelse(!date_is_recoded & !is.na(value) & is.na(date), "flag_ambiguous", NA)) %>% 
    select(-date_is_recoded)
  

  #### Other date-logic checks -------------------------------------------------
  
  # define flag-variable mappings
  df_flags <- tibble(
    flag = c(
      rep("flag_upload_before_report", 2),
      rep("flag_outcome_before_consult", 2)
    ),
    variable = c(
      "upload_date", "report_date",
      "outcome_date_of_outcome", "MSF_date_consultation"
    ),
    value = TRUE, check_date = TRUE
  )
  
  dat_date_flags <- dat_date %>%
    select(-value, -flag_ambiguous) %>%
    tidyr::spread(variable, date) %>% 
    mutate(
      flag_upload_before_report = upload_date < report_date,
      flag_outcome_before_consult = as.numeric(outcome_date_of_outcome - MSF_date_consultation) < -5,
    ) %>% 
    select(db_row, patient_id, starts_with("flag")) %>% 
    tidyr::gather(flag, value, -db_row, -patient_id) %>% 
    mutate(value = ifelse(!value, NA, value)) %>% 
    left_join(df_flags, by = c("flag", "value")) %>% 
    filter(value) %>% 
    group_by(db_row, patient_id, variable) %>% 
    summarize(flag = paste(unique(flag), collapse = "; "), .groups = "keep") %>% 
    ungroup()

  dates_check <- dat_date %>% 
    mutate(flag_future = ifelse(date > lubridate::today(), "flag_future", NA_character_),
           flag_too_early = ifelse(date < as.Date("2020-02-01"), "flag_too_early", NA_character_)) %>% 
    left_join(dat_date_flags, by = c("db_row", "patient_id", "variable")) %>%
    tidyr::unite("flag", flag_ambiguous, flag_too_early, flag_future, flag, na.rm = TRUE, sep = "; ") %>% 
    mutate(flag = ifelse(flag == "", NA_character_, gsub("flag_", "", flag))) %>% 
    anti_join(dict_dates_ignore, by = c("patient_id", "variable", "flag")) %>% 
    group_by(db_row, patient_id) %>% 
    mutate(check = any(!is.na(flag))) %>% 
    ungroup() %>% 
    filter(check) %>% 
    select(-check) %>% 
    filter(!is.na(value) | !is.na(date)) %>% 
    arrange(db_row, date) %>% 
    mutate(date = as.character(date),
           date_correct = NA_character_,
           comment = NA_character_) %>% 
    mutate(date_correct = ifelse(flag == "ambiguous", ".na", NA_character_)) %>% 
    select(patient_id, variable, value, date, date_correct, flag, comment)
  

  ## check for non-missing values not converted to date
  if (nrow(dates_check) > 0 & write_checks) {
    
    llutils::write_simple_xlsx(
      dates_check,
      file = file.path(path_corrections_dates, glue("dates_check_compiled_{time_stamp()}.xlsx")),
      group = patient_id
    )
    
    nambig <- sum(!is.na(dates_check$flag), na.rm = TRUE)
    message(paste(nambig, "date problems written to file"))
  }
  
  
  ## merge cleaned date columns back into dat
  dat_dates_clean <- dat_date %>%
    select(-value, -flag_ambiguous) %>%
    tidyr::spread(variable, date) %>% 
    left_join_replace(x = dat_numeric_clean, y = ., cols_match = c("db_row", "patient_id"))
  
  
  #### Clean categorical variables ---------------------------------------------
  
  dict_factors_long <- dict_factors %>% 
    mutate(english = values_en) %>% 
    pivot_longer(cols = starts_with("values"), names_to = "language") %>% 
    mutate(language = stringr::str_sub(language, -2)) %>% 
    arrange(language) %>% 
    mutate(value_std = hmatch::string_std(value)) %>% 
    select(value_std, value, variable, english, language)
  
  dat_factors_clean <- dat_dates_clean %>% 
    mutate(language = tolower(stringr::str_sub(linelist_lang, 1, 2))) %>% 
    mutate(language = recode(language, "po" = "pt")) %>% 
    mutate_at(unique(dict_factors$variable), hmatch::string_std) %>% 
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
    matchmaker::match_df(dict_countries_correct)
  
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
  dat_countries_clean$age_in_years <- llutils::age_to_years(
    dat_countries_clean$patinfo_ageonset,
    dat_countries_clean$patinfo_ageonsetunit
  )
  
  
  ### Update patcourse_admit and patcourse_presHCF
  dat_out <- dat_countries_clean %>% 
    mutate(
      patcourse_admit = case_when(
        MSF_visit_type %in% c("Admission to isolation center", "First hospitalisation", "First hospitalisation after a consultation", "Rehospitalisation") ~ "Yes",
        TRUE ~ patcourse_admit
      ),
      patcourse_presHCF = case_when(
        patcourse_admit == "Yes" & is.na(patcourse_presHCF) ~ MSF_date_consultation,
        TRUE ~ patcourse_presHCF
      )
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

