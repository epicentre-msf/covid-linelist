#' Import AFG_P_GZG from REDCap clinical linelist
#' 
#' 
import_other_afg_redcap <- function(path_linelist_other, dict_linelist) {
  
  ## requires ------------------------------------------------------------------
  source("R/zzz.R")
  source("R/utilities.R")
  library(redcap)  # remotes::install_github("epicentre-msf/redcap")

  
  ## mapping files -------------------------------------------------------------
  path_to_files <- file.path(path_linelist_other, "OCP", "AFG REDCap")
  
  df_map <- file.path(path_to_files, "LL_v3.0_mapping_AFG_REDCap.xlsx") %>% 
    readxl::read_xlsx() %>% 
    janitor::clean_names() %>% 
    select(1, 7, 8, 9, 10) %>% 
    setNames(c("var_epi", "map_type", "map_direct", "map_constant", "map_derive"))
  
  df_map_direct <- df_map %>% 
    filter(map_type == "1:1 correspondence") %>% 
    select(var_epi, map_direct)
  
  vec_map_direct <- setNames(df_map_direct$map_direct, df_map_direct$var_epi)
  
  df_map_constant <- df_map %>% 
    filter(map_type == "Constant value") %>% 
    select(var_epi, map_constant) %>% 
    tidyr::pivot_wider(names_from = "var_epi", values_from = "map_constant")
  
  df_map_derive <- df_map %>% 
    filter(map_type == "Requires derivation") %>% 
    select(var_epi, map_derive)
  
  dict_facilities_join <- dict_facilities %>% 
    mutate_all(as.character) %>% 
    select(site, country, shape, OC, project, site_name, site_type, uid)

  
  ## API connection ------------------------------------------------------------
  conn_afg <- redcap::rconn(
    url = "https://redcap.msf.fr/api/",
    token = Sys.getenv("COVID_AFG")
  )
  
  
  ## fetch and prep REDCap db --------------------------------------------------
  db <- redcap::fetch_database(conn_afg)
  
  db_testing <- db$covid_testing %>% 
    filter(redcap_repeat_instance == 1)
  
  d_orig <- db$initial_assessment %>%
    mutate(
      site = "AFG_P_GZG",
      upload_date = Sys.Date(),
    ) %>% 
    select(-starts_with("redcap")) %>% 
    left_join(select(db_testing, -starts_with("redcap")), by = "rec") %>%
    left_join(select(db$discharge, -starts_with("redcap")), by = "rec") %>%
    left_join(dict_facilities_join, by = "site")
  
  
  ## derived variables ---------------------------------------------------------
  test_set_equal(d_orig$cvd_method, c("pcr", "antigen detection", "other", NA))
  test_set_equal(d_orig$cvd_result, c("positive", "negative", NA))
  
  d_derive <- d_orig %>% 
    mutate(
      # patinfo_ageonsetunit
      patinfo_ageonsetunit = if_else(!is.na(pat_age), "Year", NA_character_),
      # MSF_admin_location_past_week
      MSF_admin_location_past_week = gsub("\\/", "|", pat_residence_adm),
      MSF_admin_location_past_week = dplyr::na_if(MSF_admin_location_past_week, "Other"),
      # patinfo_idadmin1
      patinfo_idadmin1 = map_chr(pat_residence_adm, ~ strsplit(.x, " / ")[[1]][1]),
      patinfo_idadmin1 = dplyr::na_if(patinfo_idadmin1, "Other"),
      # MSF_covid_status
      MSF_covid_status = dplyr::coalesce(ext_diagnosis, pat_diagnosis),
      # MSF_test_type
      MSF_test_type = case_when( # may need to update
        tolower(cvd_method) %in% "pcr" ~ "PCR",
        tolower(cvd_method) %in% "antigen detection" ~ "RDT antigen",
        tolower(cvd_method) %in% "other" ~ "Others",
        TRUE ~ NA_character_
      ),
      # MSF_test_results
      MSF_test_results = cvd_result, # may need to update
      # patcourse_dateonset
      patcourse_dateonset = pat_arrival_dt - enr_stm_onset_days,
      # MSF_symptom_aches
      MSF_symptom_aches = case_when(
        enr_stm_pain_muscle %in% "Yes" | enr_stm_pain_joint %in% "Yes" ~ "Yes",
        enr_stm_pain_muscle %in% "No" | enr_stm_pain_joint %in% "No" ~ "No",
        is.na(enr_stm_pain_muscle) & is.na(enr_stm_pain_muscle) ~ NA_character_,
        TRUE ~ "Unkown"
      ),
      # MSF_hiv_status
      MSF_hiv_status = case_when(
        cmb_hiv %in% "Yes" & cmb_hiv_arv %in% "Yes" ~ "Positive (on ART)",
        cmb_hiv %in% "Yes" & cmb_hiv_arv %in% "No" ~ "Positive (no ARV)",
        cmb_hiv %in% "Yes" ~ "Positive (unknown)",
        cmb_hiv %in% "No" ~ "Negative",
        is.na(cmb_hiv) ~ NA_character_,
        TRUE ~ "Unknown"
      ),
      # MSF_malaria
      MSF_malaria = case_when(
        enr_malaria %in% "+" ~ "Positive",
        enr_malaria %in% "-" ~ "Negative",
        is.na(enr_malaria) ~ NA_character_,
        TRUE ~ "Unkown"
      ),
      # MSF_smoking
      MSF_smoking = case_when(
        bkg_smoker == "Current smoker" ~ "Yes (current)",
        bkg_smoker == "Former Smoker" ~ "Yes (former)",
        bkg_smoker == "Non-Smoker" ~ "No",
        TRUE ~ NA_character_
      ),
      # MSF_visit_type
      MSF_visit_type = case_when(
        enr_dispose %in% "Admitted to Hospital" ~ "First hospitalisation",
        is.na(enr_dispose) ~ NA_character_,
        TRUE ~ "Other"
      ),
      # patcourse_admit
      patcourse_admit = case_when(
        enr_dispose %in% "Admitted to Hospital" ~ "Yes",
        is.na(enr_dispose) ~ NA_character_,
        TRUE ~ "No"
      ),
      # patcourse_presHCF
      patcourse_presHCF = case_when(
        enr_dispose %in% "Admitted to Hospital" ~ pat_arrival_dt,
        TRUE ~ as.Date(NA)
      ),
      # patcourse_icu
      patcourse_icu = case_when(
        enr_dispose_service %in% "ICU" ~ "Yes",
        is.na(enr_dispose_service) ~ "Unknown",
        TRUE ~ "No"
      ),
      # outcome_patcourse_status
      outcome_patcourse_status = case_when(
        otc_discharge %in% "Death" ~ "Died",
        MSF_covid_status %in% "Confirmed" & otc_discharge %in% "Discharge at Home" ~ "Cured",
        otc_discharge %in% "Discharge at Home" ~ "Sent back home",
        otc_discharge %in% c("Transfer to another hospital", "Referral to Convalescence Unit") ~ "Transferred",
        otc_discharge %in% c("Left Against Medical Advice", "Escape") ~ "Left against medical advice",
        TRUE ~ NA_character_
      ),
      # MSF_outcome_tested
      MSF_outcome_tested = case_when(
        !is.na(cvd_smp_dt) ~ "Yes",
        is.na(cvd_smp_dt) ~ "No",
        TRUE ~ NA_character_
      )
    )
  
  
  ## constants and 1:1 mappings ------------------------------------------------
  d_out <- d_derive %>% 
    map_columns(., vec_map_direct) %>% 
    select(-any_of(names(df_map_constant))) %>%
    dplyr::bind_cols(df_map_constant, .)
  
  
  ## derived columns -----------------------------------------------------------
  cols_derive <- c(
    "db_row",
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
    "patient_id"
  )
  
  
  ## add metadata and patient_id -----------------------------------------------
  df_data <- d_out %>% 
    mutate(
      linelist_row = 1:n(),
      patient_id = paste(site, format_text(MSF_N_Patient), sep = "_"),
      db_row = 1:n(),
      linelist_lang = "English",
      linelist_vers = "Other"
    )
  
  
  ## columns to add (from original ll template) --------------------------------
  ll_template <- dict_linelist$code_name
  cols_to_add <- setdiff(c(cols_derive, ll_template), names(df_data))
  df_data[cols_to_add] <- NA_character_
  
  # check for new columns to be manually renamed
  extra_cols <- grep("^extra__", names(df_data), value = TRUE)
  new_cols <- setdiff(names(df_data), c(cols_derive, ll_template, extra_cols))
  
  
  ## return --------------------------------------------------------------------
  dplyr::select(df_data, all_of(cols_derive), all_of(ll_template), starts_with("extra_"))
}




#### compare intersectional ll to clinical DB ----------------------------------
if (FALSE) {
  
  source("R/zzz.R")
  source("R/utilities.R")
  source("R/import_other_afg_redcap.R")
  source("R/clean.R")
  library(qxl) # remotes::install_github("epicentre-msf/qxl")
  
  
  ## import via REDCap clinical DB
  d_redcap <- import_other_afg_redcap(path_linelist_other, dict_linelist)
  
  path_onedrive <- file.path(path_linelist_other, "OCP", "AFG REDCap")
  
  # write to file (pre-cleaning and geomatching)
  qxl::qxl(
    d_redcap,
    file.path(path_onedrive, glue::glue("compiled_from_clinical_{Sys.Date()}.xlsx"))
  )
  
  ## import from intersectional linelist
  d_linelist <- list.files(
    path_export_global,
    pattern = "^msf_covid19_linelist_global_.*\\.rds",
    full.names = TRUE
  ) %>% max() %>% 
    readRDS() %>%
    filter(site == "AFG_P_GZG") %>% 
    dplyr::select(any_of(dict_linelist$code_name)) %>% 
    select(-starts_with("MSF_variable_additional"))
  
  # compare date range
  d_redcap %>% pull(MSF_date_consultation) %>% range()
  d_linelist %>% pull(MSF_date_consultation) %>% range()
  
  # make sure no common patient ID when clinical DB filtered to after date of last entry in intersect. ll
  # last entry from intersectional 2021-02-27, then clinical restarts 2021-06-09
  d_redcap_after <- d_redcap %>% 
    filter(MSF_date_consultation > max(d_linelist$MSF_date_consultation))
  
  intersect(d_redcap_after$MSF_N_Patient, d_linelist$MSF_N_Patient)
  intersect(d_linelist$MSF_N_Patient, d_redcap_after$MSF_N_Patient)
  
  ## compare -------------------------------------------------------------------
  d_redcap_clean <- d_redcap %>% 
    mutate(across(everything(), as.character)) %>% 
    clean_linelist(
      path_dictionaries,
      path_corrections_dates,
      vars_date = vars_date,
      vars_numeric = vars_numeric,
      dict_factors = dict_factors,
      dict_countries = dict_countries,
      corr_numeric = corr_numeric,
      dict_factors_correct,
      dict_countries_correct,
      write_checks = FALSE
    ) %>% 
    dplyr::select(any_of(dict_linelist$code_name)) %>% 
    select(-starts_with("MSF_variable_additional"))
  
  d_redcap_long <- d_redcap_clean %>% 
    mutate(across(everything(), as.character)) %>% 
    tidyr::pivot_longer(cols = -MSF_N_Patient, values_to = "value_redcap")
  
  d_linelist_long <- d_linelist %>% 
    mutate(across(everything(), as.character)) %>% 
    tidyr::pivot_longer(cols = -MSF_N_Patient, values_to = "value_linelist")
  
  d_compare <- d_linelist_long %>% 
    inner_join(d_redcap_long, by = c("MSF_N_Patient", "name")) %>% 
    # filter(!(is.na(value_linelist) & is.na(value_redcap))) %>% 
    mutate(
      across(c(value_linelist, value_redcap), ~ if_else(tolower(.x) == "unknown", NA_character_, .x)),
      value_redcap = case_when(
        value_redcap %in% "Herat" ~ "Hirat هرات",
        value_redcap %in% "nasal" ~ "Nasal swab",
        value_redcap %in% "MSF - HRH Triage" ~ "HRH Triage (Herat Regional Hospital)",
        TRUE ~ value_redcap
      )
    ) %>% 
    mutate(
      name = factor(name, levels = unique(dict_linelist$code_name)),
      missing_both = is.na(value_linelist) & is.na(value_redcap),
      missing_linelist = is.na(value_linelist) & !is.na(value_redcap),
      missing_redcap = is.na(value_redcap) & !is.na(value_linelist),
      present_both_equal = value_linelist == value_redcap,
      present_both_different = value_linelist != value_redcap
    )
  
  d_compare_out <- d_compare %>% 
    group_by(name) %>% 
    summarize(
      missing_both = sum(missing_both, na.rm = TRUE),
      missing_linelist = sum(missing_linelist, na.rm = TRUE),
      missing_redcap = sum(missing_redcap, na.rm = TRUE),
      present_both_equal = sum(present_both_equal, na.rm = TRUE),
      present_both_different = sum(present_both_different, na.rm = TRUE),
      .groups = "drop"
    ) %>% 
    left_join(select(dict_linelist, name = code_name, label = variable_short_label), by = "name") %>% 
    relocate(label, .after = name)
  
  values_compare_out <- d_compare %>% 
    filter(present_both_different | missing_linelist | missing_redcap) %>% 
    count(name, value_linelist, value_redcap, sort = TRUE) %>% 
    filter(n > 3) %>% 
    left_join(select(dict_linelist, name = code_name, label = variable_short_label), by = "name") %>% 
    relocate(label, .after = name)
  
  missing_from_redcap <- d_linelist %>% 
    filter(!MSF_N_Patient %in% c("G207004", "G207006")) %>% # in REDCap as G207004_G207006
    filter(MSF_covid_status == "Confirmed") %>% 
    anti_join(d_redcap, by = "MSF_N_Patient") %>% 
    select(MSF_N_Patient, MSF_date_consultation, MSF_covid_status)
  
  missing_from_linelist <- d_redcap %>% 
    filter(!MSF_N_Patient %in% c("G207004_G207006")) %>% 
    anti_join(d_linelist, by = "MSF_N_Patient") %>% 
    select(MSF_N_Patient, MSF_date_consultation, MSF_covid_status)
  
  qxl::qxl(
    list(
      "Missing REDCap" = missing_from_redcap,
      "Missing Linelist" = missing_from_linelist,
      "Comparison" = d_compare_out,
      "Discrepancies" = values_compare_out
    ),
    file = file.path(path_onedrive, glue::glue("intersectional_vs_clinical_{Sys.Date()}.xlsx"))
  )
}

