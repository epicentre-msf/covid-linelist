#' Import, standardize, and combine linelists from each facility
#'
#' @param country Country ISO code
#' @param path_data_raw Path to directory containing linelists
#' @param dict_facilities Dictionary mapping site-ID columns (country, OC,
#'   project) to site codes
#' @param dict_linelist Master linelist variable dictionary
#'
#' @return
#' Combined linelist <tibble> created by binding together the most recent
#' linelist version for each facility, with minor cleaning (e.g. removing
#' almost-empty lines) and standardizing (e.g. variable names)
#' 
import_other_yem_ocb <- function(path_linelist_other, dict_linelist) {
  
  ## requires
  library(dplyr)
  library(hmatch)
  
  path_to_files <- file.path(path_linelist_other, "OCB", "YEM")
  
  df_map <- file.path(path_to_files, "LL_v2.1_mapping_template_OCB_YEM.xlsx") %>% 
    readxl::read_xlsx() %>% 
    janitor::clean_names() %>% 
    select(1, 6, 7, 8, 9, 10) %>% 
    setNames(c("var_epi", "category", "map_type", "map_direct", "map_constant", "map_derive"))
  
  df_map_direct <- df_map %>% 
    filter(map_type == "1:1 correspondence") %>% 
    select(var_epi, map_direct) %>% 
    mutate(map_direct_std = map_direct)
  
  vec_map_direct <- setNames(df_map_direct$map_direct_std, df_map_direct$var_epi)
  
  df_map_constant <- df_map %>% 
    filter(map_type == "Constant value", !category %in% "Site metadata") %>% 
    select(var_epi, map_constant) %>% 
    tidyr::pivot_wider(names_from = "var_epi", values_from = "map_constant")
  
  df_map_derive <- df_map %>% 
    filter(map_type == "Requires derivation") %>% 
    select(var_epi, map_derive)
  
  file_ll <- llutils::list_files(
    path_to_files,
    pattern = "Al Gam.*\\.xlsx",
    ignore.case = TRUE,
    full.names = TRUE,
    select = "latest"
  )
  
  dict_facilities_join <- dict_facilities %>% 
    mutate_all(as.character) %>% 
    select(site, country, shape, OC, project, site_name, site_type, uid)
  
  d_orig <- readxl::read_xlsx(file_ll, col_types = "text", skip = 1, .name_repair = hmatch::string_std) %>% 
    rename("Comcond_other" = "") %>%  # original variables has no English name
    janitor::remove_empty("rows") %>% 
    # janitor::clean_names() %>% 
    mutate(site = "YEM_B_GAM") %>% 
    dplyr::left_join(dict_facilities_join, by = "site") %>% 
    mutate(upload_date = as.character(llutils::extract_date(file_ll)))

  
  ### Check for unseen values in derivation variables
  test_set_equal(d_orig$the_test_result_negative_positive_not_done_pending, c("negative", "positive", "pending", NA))
  test_set_equal(d_orig$diagnostic_mechanism, c("ct.scan", "ct,scan", "chest.x-ray", "x-ray", NA))
  test_set_equal(d_orig$cardiovascular_disease_yes_no, c("yes", "no", NA))
  test_set_equal(d_orig$if_she_is_pregnant_in_which_month_no, c(NA)) # reassess derivation if values populated
  
  ### Derivation for patcourse_asymp
  any_symptoms <- d_orig %>% 
    select(
      high_temperature_yes_no,
      sore_throat_yes_no,
      difficulty_breathing_yes_no,
      muscle_and_joint_pain_yes_no,
      descent_from_the_nose_yes_no,
      cough_yes_no
    ) %>% 
    mutate_all(tolower) %>% 
    apply(., 1, function(x) any(grepl("^y", x)))
  
  
  ### Derived variables
  d_derive <- d_orig %>% 
    # derive patinfo_ageonset and patinfo_ageonsetunit
    mutate(
      patinfo_ageonset = case_when(
        !is.na(age_in_years) ~ age_in_years,
        !is.na(age_in_months) ~ age_in_months,
        !is.na(age_in_days) ~ age_in_days,
      ),
      patinfo_ageonsetunit = case_when(
        !is.na(age_in_years) ~ "Year",
        !is.na(age_in_months) ~ "Month",
        !is.na(age_in_days) ~ "Day",
      )
    ) %>% 
    # derive MSF_admin_location_past_week
    mutate(across(case_governorate:name_of_the_area, ~ ifelse(is.na(.x), "", .x))) %>% 
    unite("MSF_admin_location_past_week", case_governorate:name_of_the_area, sep = " | ") %>% 
    # derive MSF_covid_status
    mutate(test_result = the_test_result_negative_positive_not_done_pending) %>% 
    mutate(across(c(test_result, diagnostic_mechanism), tolower)) %>% 
    mutate(MSF_covid_status = case_when(
      test_result %in% "positive" | diagnostic_mechanism %in% c("ct.scan", "ct,scan", "chest.x-ray") ~ "Confirmed",
      test_result == "negative" & !diagnostic_mechanism %in% c("ct.scan", "ct,scan", "chest.x-ray") ~ "Not a case",
      TRUE ~ "Probable"
    )) %>% 
    # derive patcourse_asymp
    mutate(
      any_symptoms = any_symptoms,
      patcourse_asymp = case_when(
        any_symptoms | !is.na(onset_date) ~ "No",
        TRUE ~ "Yes"
      )
    ) %>% 
    # derive Comcond_pregt
    mutate(Comcond_pregt = case_when(
      as.numeric(if_she_is_pregnant_in_which_month_no) %in% 0:3 ~ "1",
      as.numeric(if_she_is_pregnant_in_which_month_no) %in% 4:6 ~ "2",
      as.numeric(if_she_is_pregnant_in_which_month_no) %in% 7:9 ~ "3",
      TRUE ~ NA_character_
    )) %>% 
    # derive Comcond_cardi
    mutate(across(c(cardiovascular_disease_yes_no, blood_pressure_yes_no), tolower)) %>% 
    mutate(
      Comcond_cardi = case_when(
        cardiovascular_disease_yes_no == "yes" | blood_pressure_yes_no == "yes" ~ "Yes",
        cardiovascular_disease_yes_no == "no" & blood_pressure_yes_no == "no" ~ "No",
        TRUE ~ NA_character_
      )
    ) %>% 
    # derive outcome_patcourse_status
    mutate(outcome_status = outcome_of_the_patient_discharged_dead_under_treatment_escaped_referred) %>% 
    mutate(across(c(outcome_status), tolower)) %>% 
    mutate(
      outcome_patcourse_status = case_when(
        grepl("disc", outcome_status) ~ "Sent back home",
        TRUE ~ outcome_status
      )
    )
    
  
  ### Constants and 1:1 mappings
  d_out <- d_derive %>% 
    rename(all_of(vec_map_direct)) %>% 
    select(-any_of(names(df_map_constant))) %>% # reassess
    dplyr::bind_cols(df_map_constant, .) %>% 
    # copied outcome variables
    mutate(
      patcourse_presHCF = MSF_date_consultation,
      outcome_patcourse_presHCF = MSF_date_consultation
    )
  
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
    group_by(site) %>% 
    mutate(linelist_row = 1:n()) %>% 
    ungroup() %>% 
    mutate(patient_id = paste(site, format_text(MSF_N_Patient), sep = "_")) %>% 
    mutate(db_row = 1:n()) %>% 
    mutate(linelist_lang = "English",
           linelist_vers = "Other")
  
  ## columns to add (from original ll template)
  ll_template <- dict_linelist$code_name
  cols_to_add <- setdiff(c(cols_derive, ll_template), names(df_data))
  df_data[cols_to_add] <- NA_character_
  
  # check for new columns to be manually renamed
  extra_cols <- grep("^extra__", names(df_data), value = TRUE)
  new_cols <- setdiff(names(df_data), c(cols_derive, ll_template, extra_cols))
  
  ## return
  dplyr::select(df_data, all_of(cols_derive), all_of(ll_template), starts_with("extra_"))
}
 

