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
import_other_tun_ocb <- function(path_linelist_other, dict_linelist, date_cutoff = NULL) {
  
  ## requires
  library(dplyr)
  library(hmatch)
  source("R/import_other_jor_ocp.R")
  
  dict_facilities_join <- dict_facilities %>% 
    mutate_all(as.character) %>% 
    select(site, country, shape, OC, project, site_name, site_type, uid)

  path_to_files <- file.path(path_linelist_other, "OCB", "TUN")
  
  df_map <- file.path(path_to_files, "LL_v3.0_mapping_Tunisia Beja Hospital.xlsx") %>% 
    readxl::read_xlsx() %>% 
    janitor::clean_names() %>% 
    select(1, 7, 8, 9, 10) %>% 
    setNames(c("var_epi", "map_type", "map_direct", "map_constant", "map_derive"))
  
  df_map_direct <- df_map %>% 
    filter(map_type == "1:1 correspondence") %>% 
    select(var_epi, map_direct) %>% 
    mutate(map_direct_std = hmatch::string_std(map_direct))
  
  vec_map_direct <- setNames(df_map_direct$map_direct_std, df_map_direct$var_epi)
  
  df_map_constant <- df_map %>% 
    filter(map_type == "Constant value") %>% 
    select(var_epi, map_constant) %>% 
    tidyr::pivot_wider(names_from = "var_epi", values_from = "map_constant")
  
  df_map_derive <- df_map %>% 
    filter(map_type == "Requires derivation") %>% 
    select(var_epi, map_derive)
  
  files_ll <- llutils::list_files(
    path_to_files,
    pattern = "^BEJA HOPITAL DE COMPAGNE.*\\.xlsx",
    select = "latest"
  )
  
  # read
  d_orig <- purrr::map2_dfr(
    files_ll,
    "TUN_B_BEJ",
    import_tun_ocb_
  ) %>% 
    select(-any_of(c("country", "OC", "project", "site_type", "site_name"))) %>% 
    left_join(dict_facilities, by = "site")
  
  d_orig %>% 
    count(country)
  
  dict_facilities %>% 
    filter(site == "TUN_B_BEJ")
  
  ### Check for unseen values in derivation variables
  test_set_equal(d_orig$covid_19_pcr_status_positive_negative_not_done, c("negative", "positive", "not done", "yes", NA))
  test_set_equal(d_orig$ct_scan_typical_for_covid_yes_no_not_done, c("yes", "no", "not done", NA))
  test_set_equal(
    d_orig$oxygen_therapy_maximum_amount_single_nasal_canula_or_mask_at_2_to_20_l_min_or_double_non_rebreather_mask_nasal_canula_20l_min,
    c("non-rebreather mask (nrm) + nasal canula (nc) >20l/min", "nasal canula or mask at 2 to 20 l/min", NA)
  )
  test_set_equal(
    d_orig$exit_status_death_transfer_to_icu_referral_to_another_hospital_discharged_to_outpatient_care_discharged_cured_lama,
    c("referral to another hospital", "lama", "transfer to icu", "discharged cured", "discharged to outpatient care", "death", NA)
  )
  test_set_equal(d_orig$admission_to_hospital_yes_no_readmission, c("readmission", "yes", "no", NA))
  
  ### Derived variables
  d_derive <- d_orig %>% 
    rename(
      oxygen_therapy = oxygen_therapy_maximum_amount_single_nasal_canula_or_mask_at_2_to_20_l_min_or_double_non_rebreather_mask_nasal_canula_20l_min ,
      exit_status = exit_status_death_transfer_to_icu_referral_to_another_hospital_discharged_to_outpatient_care_discharged_cured_lama
    ) %>% 
    mutate(
      MSF_test_type = dplyr::if_else(
        tolower(covid_19_pcr_status_positive_negative_not_done) %in% c("negative", "positive", "not done"),
        "PCR",
        NA_character_
      ),
      MSF_test_other_type = dplyr::if_else(
        tolower(ct_scan_typical_for_covid_yes_no_not_done) %in% c("yes", "no"),
        "CT Scan",
        NA_character_
      ),
      MSF_test_other_result = dplyr::case_when(
        tolower(ct_scan_typical_for_covid_yes_no_not_done) %in% "yes" ~ "Suggestive of C19",
        tolower(ct_scan_typical_for_covid_yes_no_not_done) %in% "no" ~ "Non-suggestive of C19",
        TRUE ~ NA_character_
      ),
      MSF_visit_type = dplyr::case_when(
        tolower(admission_to_hospital_yes_no_readmission) %in% "readmission" ~ "Rehospitalization",
        tolower(transer_from_icu_yes_no) %in% "yes" ~ "Other",
        TRUE ~ "First hospitalization"
      ),
      MSF_received_oxygen = dplyr::case_when(
        grepl("nasal canula", tolower(oxygen_therapy)) ~ "Yes",
        TRUE ~ "No"
      ),
      MSF_outcome_received_oxygen = dplyr::case_when(
        grepl("nasal canula", tolower(oxygen_therapy)) ~ "Yes",
        TRUE ~ "No"
      ),
      outcome_patcourse_status = dplyr::case_when(
        tolower(exit_status) %in% "discharged cured" ~ "Cured",
        tolower(exit_status) %in% "discharged to outpatient care" ~ "Cured",
        tolower(exit_status) %in% "lama" ~ "Left against medical advice",
        tolower(exit_status) %in% "referral to another hospital" ~ "Transferred",
        tolower(exit_status) %in% "transfer to icu" ~ "Other",
        tolower(exit_status) %in% "death" ~ "Died"
      )
    )
  
  ### Constants and 1:1 mappings
  d_out <- d_derive %>% 
    map_columns(., vec_map_direct) %>% 
    select(-any_of(names(df_map_constant))) %>%
    dplyr::bind_cols(df_map_constant, .)
  
  ## derived columns
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
  
  ## import and prepare
  df_data <- d_out %>% 
    group_by(site) %>% 
    mutate(linelist_row = 1:n()) %>% 
    ungroup() %>% 
    mutate(patient_id = paste(site, format_text(MSF_N_Patient), sep = "_")) %>% 
    mutate(db_row = 1:n()) %>% 
    mutate(linelist_lang = ll_language,
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



import_tun_ocb_ <- function(path, site) {
  
  readxl::read_xlsx(
    path, 
    col_types = "text",
    na = c("", "NA"),
    skip = 1,
    .name_repair = ~ vctrs::vec_as_names(..., repair = "unique", quiet = TRUE)
  ) %>% 
    janitor::clean_names() %>% 
    janitor::remove_empty("rows") %>% 
    filter(!is.na(patient_id_number)) %>% 
    mutate(
      linelist_row = 1:n(),
      upload_date = as.character(llutils::extract_date(path)),
      site = site
    )
}

