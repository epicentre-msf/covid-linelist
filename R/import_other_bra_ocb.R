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
import_other_bra_ocb <- function(path_linelist_other, dict_linelist) {
  
  ## requires
  library(dplyr)
  library(hmatch)
  source("R/import_other_bra_ocb.R")
  
  dict_facilities_join <- dict_facilities %>% 
    mutate_all(as.character) %>% 
    select(site, country, shape, OC, project, site_name, site_type, uid)

  path_to_files <- file.path(path_linelist_other, "OCB", "BRA")
  
  df_map <- file.path(path_to_files, "LL_v3.0_mapping_template_Manaus.xlsx") %>% 
    readxl::read_xlsx() %>% 
    janitor::clean_names() %>% 
    select(1, 7, 8, 9, 10) %>% 
    setNames(c("var_epi", "map_type", "map_direct", "map_constant", "map_derive"))
  
  df_map_direct_raw <- df_map %>% 
    filter(map_type == "1:1 correspondence") %>% 
    select(var_epi, map_direct)
  
  df_map_direct <- df_map_direct_raw %>% 
    select(map_direct) %>% 
    unique() %>% 
    mutate(map_direct_std = janitor::make_clean_names(map_direct)) %>% 
    left_join(df_map_direct_raw, ., by = "map_direct")
  
  vec_map_direct <- setNames(
    df_map_direct$map_direct_std,
    df_map_direct$var_epi
  )
  
  df_map_constant <- df_map %>% 
    filter(map_type == "Constant value") %>% 
    select(var_epi, map_constant) %>% 
    tidyr::pivot_wider(names_from = "var_epi", values_from = "map_constant")
  
  df_map_derive <- df_map %>% 
    filter(map_type == "Requires derivation") %>% 
    select(var_epi, map_derive)
  
  ll_path <- llutils::list_files(
    path_to_files,
    pattern = "MSF ICU line.*\\.xlsx",
    select = "latest"
  )
  
  d_orig <- readxl::read_xlsx(
    ll_path,
    skip = 3,
    col_types = "text",
    na = c("", "NA"),
    .name_repair = ~ vctrs::vec_as_names(..., repair = "unique", quiet = TRUE)
  ) %>% 
    slice(-1) %>% 
    janitor::clean_names() %>% 
    janitor::remove_empty("rows") %>% 
    filter(apply(., 1, function(x) sum(!is.na(x)) > 5)) %>%  # remove mostly-empty rows
    mutate(
      linelist_row = 1:n(),
      upload_date = as.character(llutils::extract_date(ll_path)),
      site = "BRA_B_UJR",
      .before = 1
    ) %>% 
    rename(
      "non_invasive_end" = "x13",
      "non_invasive_days" = "x14",
      "invasive_end" = "x17",
      "invasive_days" = "x18"
    ) %>% 
    dplyr::left_join(dict_facilities_join, by = "site")
  
  ### Check for unseen values in derivation variables
  test_set_equal(
    d_orig$repeat_admisison_to_icu,
    c("Yes", "No", NA)
  )
  test_set_equal(
    d_orig$non_invasive_ventilation_bi_pap_or_cpap,
    c("Yes", "No", NA)
  )
  test_set_equal(
    d_orig$high_flow_oxygen_therapy_yes_no_specificy_type_nrm_nc_or_hfno,
    c(NA)
  )
  test_set_equal(
    d_orig$exit_status,
    c("Death", "Referral to another hospital", "Transfer to IPD", NA)
  )
  
  ### Derived variables
  d_derive <- d_orig %>% 
    mutate(
      patinfo_ageonsetunit = if_else(!is.na(age), "Year", NA_character_),
      MSF_test_type = if_else(!is.na(covid_19_pcr_status), "PCR", NA_character_),
      MSF_test_other_type = if_else(!is.na(covid_19_pcr_status), "CT Scan", NA_character_),
      # MSF_test_other_result # omit mapping for now because all NA
      MSF_visit_type = case_when(
        repeat_admisison_to_icu == "Yes" ~ "Rehospitalization",
        TRUE ~ "First hospitalization"
      ),
      # MSF_received_oxygen # omit mapping b/c high_flow_oxygen_therapy_yes_no_specificy_type_nrm_nc_or_hfno all NA
      patcourse_vent = case_when(
        non_invasive_ventilation_bi_pap_or_cpap == "Yes" | invasive_mechanical_ventilation == "Yes" ~ "Yes",
        non_invasive_ventilation_bi_pap_or_cpap == "No" & invasive_mechanical_ventilation == "No" ~ "No",
        is.na(non_invasive_ventilation_bi_pap_or_cpap) & is.na(invasive_mechanical_ventilation) ~ NA_character_,
        TRUE ~ "Unknown"
      ),
      outcome_patcourse_vent = patcourse_vent,
      # omit mapping for _HFNC b/c high_flow_oxygen_therapy_yes_no_specificy_type_nrm_nc_or_hfno all NA
      # MSF_HFNC
      # MSF_outcome_HFNC
      # clean non_invasive_days and invasive_days (auto-populated to 1)
      non_invasive_days = as.integer(if_else(is.na(non_invasive_end), NA_character_, non_invasive_days)),
      invasive_days = as.integer(if_else(is.na(invasive_end), NA_character_, invasive_days)),
      MSF_outcome_ventilated_days = map2_chr(non_invasive_days, invasive_days, ~ sum(.x, .y, na.rm = TRUE)),
      outcome_patcourse_status = case_when(
        exit_status == "Death" ~ "Died",
        exit_status == "Referral to another hospital" ~ "Transferred",
        exit_status == "Transfer to IPD" ~ "Other",
        TRUE ~ NA_character_
      ),
      outcome_patcourse_status_other = case_when(
        exit_status == "Transfer to IPD" ~ "Transfer to IPD",
        TRUE ~ NA_character_
      )
    )
      
  
  ### Constants and 1:1 mappings
  d_out <- d_derive %>% 
    map_columns(., vec_map_direct) %>% 
    select(-any_of(names(df_map_constant))) %>%
    dplyr::bind_cols(df_map_constant, .)
  
  ### Check that Constant and 1:1 variables all mapped
  names_check <- c(names(df_map_constant), names(vec_map_direct))
  names_missing <- setdiff(names_check, names(d_out))
  if (length(names_missing) > 0) {
    warning("The following columns could not be mapped: ", paste(names_missing, collapse = "; "))
  }
  
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
    mutate(
      linelist_lang = ll_language,
      linelist_vers = "Other"
    )
  
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
 
