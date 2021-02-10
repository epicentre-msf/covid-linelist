#' Import, standardize, and combine linelists from each facility
#'
#' @param path_linelist_other 
#' @param dict_linelist Master linelist variable dictionary
#' @param exclude optional data frame containing combinations of site and MSF_N_Patient to exclude
#'
#' @return
#' Combined linelist <tibble> created by binding together the most recent
#' linelist version for each facility, with minor cleaning (e.g. removing
#' almost-empty lines) and standardizing (e.g. variable names)
#' 
import_other_bgd_godata <- function(path_linelist_other, dict_linelist, exclude = NULL) {
  
  ## requires
  library(dplyr)
  library(hmatch)
  library(rlang)
  source("R/import_other_bgd_godata.R")
  
  
  ## site metadata
  dict_facilities_join <- dict_facilities %>% 
    mutate_all(as.character) %>% 
    select(site, country, shape, OC, project, site_name, site_type, uid)
  
  ## initial import
  path_to_files <- file.path(path_linelist_other, "OCBA", "BGD")
  path_to_files_oca <- file.path(path_linelist_other, "OCA", "BGD")
  path_to_files_ocp <- file.path(path_linelist_other, "OCP", "BGD")
  
  files_ll <- c(
    BGD_E_UMS = llutils::list_files(
      path_to_files,
      pattern = "UMS.*\\.xlsx",
      select = "latest"
    ),
    BGD_E_GOY = llutils::list_files(
      path_to_files,
      pattern = "Goyalmara.*\\.xlsx",
      select = "latest"
    ),
    BGD_E_NAY = llutils::list_files(
      path_to_files,
      pattern = "Nayapara.*\\.xlsx",
      select = "latest"
    ),
    BGD_A_KUT = llutils::list_files(
      path_to_files_oca,
      pattern = "^BGD_CXB_KTP.*\\.xlsx",
      select = "latest"
    ),
    BGD_A_BAL = llutils::list_files(
      path_to_files_oca,
      pattern = "^BGD_CXB_BKL.*\\.xlsx",
      select = "latest"
    ),
    BGD_P_IPD = llutils::list_files(
      path_to_files_ocp,
      pattern = "BGD_OCP.*\\.xlsx",
      select = "latest"
    )
  )
  
  d_orig <- purrr::map2_dfr(
    files_ll,
    names(files_ll),
    import_go_data_
  ) %>% 
    filter(!(site == "BGD_P_IPD" & is.na(`_Select your agency`))) # remove old CRF rows
  
  d_name_map <- tibble(
    orig = names(d_orig),
    std = janitor::make_clean_names(names(d_orig))
  )
  
  ## mapping file
  map_occupations <- readxl::read_xlsx(file.path(path_to_files, "map_occupations.xlsx"))
  
  df_map <- file.path(path_to_files, "LL_v2.1_mapping_template_go_data.xlsx") %>% 
    readxl::read_xlsx() %>% 
    select(1, 7, 8, 9, 10) %>% 
    setNames(c("var_epi", "map_type", "map_direct", "map_constant", "map_derive"))
  
  df_map_direct <- df_map %>% 
    filter(map_type == "1:1 correspondence") %>% 
    select(var_epi, map_direct) %>% 
    mutate(map_direct_std = recode(map_direct, !!!setNames(d_name_map$std, d_name_map$orig)))
  
  vec_map_direct <- setNames(df_map_direct$map_direct_std, df_map_direct$var_epi)
  
  df_map_constant <- df_map %>% 
    filter(map_type == "Constant value") %>% 
    select(var_epi, map_constant) %>% 
    tidyr::pivot_wider(names_from = "var_epi", values_from = "map_constant")
  
  df_map_derive <- df_map %>% 
    filter(map_type == "Requires derivation") %>% 
    select(var_epi, map_derive)
  
  ### clean names and join site metadata
  d_orig <- d_orig %>% 
    janitor::clean_names() %>%
    dplyr::left_join(dict_facilities_join, by = "site") %>% 
    mutate(
      addresses_location_1_location_geographical_level_6 = case_when(
        OC == "OCP" &
          is.na(addresses_location_1_location_geographical_level_6) &
          grepl("camp|community|union", addresses_location_1, ignore.case = TRUE) ~ addresses_location_1,
        TRUE ~ addresses_location_1_location_geographical_level_6
      )
    )

  ### Check for unseen values in derivation variables
  test_set_equal(d_orig$on_treatment, c("Currently_on_treatment", "Currently no treatment", NA))
  test_set_equal(
    d_orig$msf_specific_outcome,
    c("other", "transferred", "sent back home", "lama", "admitted", "others", "ktp", "quarrentine", "not recovered", NA)
  )
  

  ### Derived variables
  ## same as date_of_current_visit?
  latest_adm_date <- d_orig %>% 
    select(starts_with("dates_start_date_")) %>% # can be more than 1
    mutate_all(as.Date) %>% 
    apply(1, max)
  
  latest_end_date <- d_orig %>% 
    select(starts_with("dates_end_date_")) %>% # can be more than 1
    mutate_all(as.Date) %>% 
    apply(1, max)
  
  extra__secondary_comcond_at_discharge <- d_orig %>% 
    select(starts_with("secondary_co_morbidities_at_discharge")) %>% 
    pmap_chr(., collapse_unique, to_chr =  TRUE) %>% 
    dplyr::na_if("NA")
  
  d_derive <- d_orig %>% 
    # patinfo_ageonset (if both ages 0, should be NA?)
    mutate(
      patinfo_ageonset = case_when(
        age_age_years == "0" & age_age_months == "0" ~ NA_character_,
        age_age_months == "0" ~ age_age_years,
        age_age_years == "0" ~ age_age_months
      )
    ) %>% 
    mutate(
      patinfo_ageonsetunit = case_when(
        age_age_years == "0" & age_age_months == "0" ~ NA_character_,
        age_age_months == "0" ~ "Year",
        age_age_years == "0" ~ "Month"
      )
    ) %>% 
    # derive MSF_admin_location_past_week
    mutate(across(addresses_location_1_location_geographical_level_3:addresses_location_1_location_geographical_level_6, ~ ifelse(is.na(.x), "", .x))) %>%
    unite("MSF_admin_location_past_week", addresses_location_1_location_geographical_level_3:addresses_location_1_location_geographical_level_6, sep = " | ") %>% 
    # MSF_job
    hmatch(map_occupations) %>% 
    # MSF_symptom_aches
    mutate(
      MSF_symptom_aches = case_when(
        muscle_ache %in% "Yes" | joint_ache %in% "Yes" ~ "Yes",
        muscle_ache %in% "No" | joint_ache %in% "No" ~ "No",
        is.na(muscle_ache) & is.na(joint_ache) ~ NA_character_,
        TRUE ~ "Unknown"
      )
    ) %>% 
    # Comcond_immuno
    mutate(
      Comcond_immuno = case_when(
        hiv_immunodeficiency %in% "Yes" | other_immunosup_disorder %in% "Yes" ~ "Yes",
        hiv_immunodeficiency %in% "No" | other_immunosup_disorder %in% "No" ~ "No",
        is.na(hiv_immunodeficiency) & is.na(other_immunosup_disorder) ~ NA_character_,
        TRUE ~ "Unknown"
      )
    ) %>% 
    # MSF_tb_active
    mutate(
      MSF_tb_active = case_when(
        tuberculosis %in% "Yes" | on_treatment %in% "Currently_on_treatment" ~ "Yes (currently on treatment)",
        tuberculosis %in% "Yes" | on_treatment %in% "Currently no treatment" ~ "Yes (currently no treatment)",
        tuberculosis %in% "Yes" & !on_treatment %in% "Currently_on_treatment" ~ "Yes (unknown)",
        tuberculosis %in% "No" ~ "No",
        tuberculosis %in% "Unknown" ~ "Unknown",
        tuberculosis %in% NA_character_ ~ NA_character_
      )
    ) %>% 
    # MSF_complications and MSF_other_complications
    mutate(
      l_complicat = purrr::pmap(
        list(
          complications, complications_1, complications_2, complications_3,
          complications_4, complications_5, complications_6, complications_7,
          complications_8, complications_9, complications_10, complications_11,
          complications_12, please_specify_other_complications
        ),
        collapse_unique
      ),
      MSF_complications = map_chr(l_complicat, extract_main_comp),
      MSF_other_complications = map_chr(l_complicat, extract_other_comp)
    ) %>% 
    # outcome_asymp
    mutate(
      outcome_asymp = case_when(
        patient_asymptomatic == "Yes" ~ "No",
        patient_asymptomatic == "No" ~ "Yes",
        TRUE ~ patient_asymptomatic
      )
    ) %>% 
    # outcome_patcourse_presHCF
    mutate(outcome_patcourse_presHCF = latest_adm_date) %>% 
    # outcome_patcourse_status
    mutate(
      outcome_patcourse_status = case_when(
        outcome == "Deceased" ~ "Died",
        outcome == "Recovered" ~ "Cured",
        is.na(msf_specific_outcome) & !outcome %in% c("Deceased", "Recovered") ~ "Other",
        tolower(msf_specific_outcome) == "lama" ~ "Left against medical advice",
        tolower(msf_specific_outcome) %in% "admitted" ~ "Other",
        tolower(msf_specific_outcome) %in% c("others", "quarrentine", "ktp", "not recovered") ~ "Other", # ???
        TRUE ~ msf_specific_outcome
      )
    ) %>% 
    # outcome_patcourse_status_other
    mutate(
      outcome_other_prep = map2_chr(outcome, please_specify_other_outcome, collapse_unique, to_chr = TRUE),
      outcome_patcourse_status_other = case_when(
        outcome_patcourse_status == "Other" & outcome %in% c("Healthy", "Not recovered") ~ outcome_other_prep,
        outcome_patcourse_status == "Other" ~ please_specify_other_outcome
      )
    ) %>% 
    # outcome_date_of_outcome
    mutate(outcome_date_of_outcome = latest_end_date) %>% 
    # other transformations
    mutate(result = dplyr::na_if(result, "Pending"),
           status_at_detection = dplyr::na_if(status_at_detection, "Asymptomatic"),
           have_you_travelled_outside_of_the_camps = dplyr::na_if(have_you_travelled_outside_of_the_camps, "Host/Not applicable")) %>% 
    ### extra
    mutate(extra__secondary_comcond_at_discharge = extra__secondary_comcond_at_discharge)
  
  ### Constants and 1:1 mappings
  d_out <- d_derive %>% 
    map_columns(., vec_map_direct) %>% 
    select(-any_of(names(df_map_constant))) %>% # reassess
    dplyr::bind_cols(df_map_constant, .)
  
  ## derived columns
  cols_derive <- c("db_row",
                   "linelist_row",
                   "upload_date",
                   "linelist_lang",
                   "linelist_vers",
                   "country",
                   "shape",
                   "OC",
                   "project",
                   "site_type",
                   "site_name",
                   "site",
                   "uid",
                   "MSF_N_Patient",
                   "patient_id")
  
  ## import and prepare
  df_data <- d_out %>% 
    # group_by(site) %>% 
    # mutate(linelist_row = 1:n()) %>% 
    # ungroup() %>% 
    mutate(patient_id = paste(site, format_text(MSF_N_Patient), sep = "_")) %>% 
    mutate(db_row = 1:n()) %>% 
    mutate(linelist_lang = "English",
           linelist_vers = "Go.Data")
  
  ## columns to add (from original ll template)
  ll_template <- dict_linelist$code_name
  cols_to_add <- setdiff(c(cols_derive, ll_template), names(df_data))
  df_data[cols_to_add] <- NA_character_
  
  # check for new columns to be manually renamed
  extra_cols <- grep("^extra__", names(df_data), value = TRUE)
  new_cols <- setdiff(names(df_data), c(cols_derive, ll_template, extra_cols))
  
  # dates
  df_data <- df_data %>% 
    mutate(across(any_of(date_vars), ~ as.character(as.Date(.x))))
  
  # exclude
  if (!is.null(exclude)) {
    df_data <- dplyr::anti_join(df_data, exclude, by = c("site", "MSF_N_Patient"))
  }
  
  ## return
  dplyr::select(df_data, all_of(cols_derive), all_of(ll_template), starts_with("extra_"))
}



import_go_data_ <- function(path, site) {
  
  d <- readxl::read_xlsx(
    path, 
    col_types = "text",
    na = c("", "NA"),
    .name_repair = ~ vctrs::vec_as_names(..., repair = "unique", quiet = TRUE)
  ) %>%
    janitor::remove_empty("rows")
  
  # remove WHO CRF entries for OCA, which overlap with data in intersectional LL
  if (site %in% c("BGD_A_KUT", "BGD_A_BAL")) {
    d <- d %>% 
      filter(!is.na(`_Health Facility`))
  }
    
  d %>% 
    mutate(linelist_row = 1:n(),
           upload_date = as.character(llutils::extract_date(path)),
           linelist_lang = "English",
           linelist_vers = "Go Data",
           site = site)
}


collapse_unique <- function(..., to_chr = FALSE) {
  x <- unique(c(..., use.names = FALSE))
  if (any(!is.na(x))) {
    x <- x[!is.na(x)]
  }
  
  if (to_chr) {
    x <- paste(x, collapse = "; ")
  }
  x
}


extract_main_comp <- function(x) {
  
  main_comp <- c(
    "Coma",
    "Convulsion",
    "Renal distress",
    "Acute respiratory distress syndrome (ARDS)"
  )
  
  if (length(x) == 1 && x %in% main_comp) {
    out <- x
  } else {
    out <- NA
  } 
  
  out
}


extract_other_comp <- function(x) {
  
  main_comp <- c(
    "Coma",
    "Convulsion",
    "Renal distress",
    "Acute respiratory distress syndrome (ARDS)"
  )
  
  if (length(x) == 1 && x %in% main_comp) {
    out <- NA
  } else if (all(is.na(x))) {
    out <- NA
  } else {
    out <- paste(x, collapse = "; ")
  }
  
  out
}

