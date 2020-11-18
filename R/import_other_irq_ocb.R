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
import_other_irq_ocb <- function(path_linelist_other, dict_linelist) {
  
  ## requires
  library(dplyr)
  library(hmatch)
  source("R/import_other_irq_ocb.R")
  
  dict_facilities_join <- dict_facilities %>% 
    mutate_all(as.character) %>% 
    select(site, country, shape, OC, project, site_name, site_type, uid)

  path_to_files <- file.path(path_linelist_other, "OCB", "IRQ")
  
  df_map <- file.path(path_to_files, "LL_v2.1_mapping_template_OCB_Mosul.xlsx") %>% 
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
  
  vec_map_direct <- setNames(df_map_direct$map_direct_std, df_map_direct$var_epi)
  
  df_map_constant <- df_map %>% 
    filter(map_type == "Constant value") %>% 
    select(var_epi, map_constant) %>% 
    tidyr::pivot_wider(names_from = "var_epi", values_from = "map_constant")
  
  df_map_derive <- df_map %>% 
    filter(map_type == "Requires derivation") %>% 
    select(var_epi, map_derive)
  
  files_ll <- c(
    IRQ_B_MOS = llutils::list_files(
      path_to_files,
      pattern = "Register Covid19_Mosul.*\\.xlsx",
      select = "latest"
    )
  )
  
  d_orig <- purrr::map2_dfr(
    files_ll,
    names(files_ll),
    import_irq_ocb_
  ) %>% 
    dplyr::left_join(dict_facilities_join, by = "site")
  
  ### Check for unseen values in derivation variables
  test_set_equal(
    d_orig$covid_status,
    c("confirmed", "suspect", NA)
  )
  test_set_equal(
    d_orig$ad_sample1_results,
    c("positive", "negative", "unknown", NA)
  )
  test_set_equal(
    d_orig$type_of_visit,
    c("first_hosp", "re-hosp", "first_hosp_after_other_cons", NA)
  )
  test_set_equal(
    d_orig$outcome,
    c("death", "other", "lama", "discharged - unknown", "discharged - cured", "discharged - isolation", "referral to hcf", "discharged - not a case", NA)
  )
  
  
  # d_derive %>%
  #   count(governorate, district, subdistrict, MSF_admin_location_past_week)
  # 
  # d_derive %>%
  #   count(MSF_visit_type, type_of_visit)
  
  # d_derive %>%
  #   count(outcome, outcome_patcourse_status)
  
  # d_derive %>% 
  #   count(MSF_covid_status, covid_status, ad_sample1_results, diagnosis_at_discharge) %>% 
  #   print(n = "all")
  
  # d_derive %>% 
  #   mutate(if_referred_for_care_isolation_hospital = tolower(if_referred_for_care_isolation_hospital)) %>% 
  #   count(MSF_refer_to, if_referred_for_care_isolation_hospital, sort = TRUE)
  
  ### Derived variables
  d_derive <- d_orig %>% 
    # derive MSF_admin_location_past_week
    mutate(across(governorate:subdistrict, ~ ifelse(is.na(.x), "", .x))) %>% 
    unite("MSF_admin_location_past_week", governorate:subdistrict, sep = " | ", remove = FALSE) %>% 
    # MSF_covid_status
    mutate(
      MSF_covid_status = case_when(
        tolower(covid_status) == "confirmed" ~ "Confirmed",
        tolower(ad_sample1_results) == "positive" ~ "Confirmed",
        tolower(diagnosis_at_discharge) == "covid-19" ~ "Confirmed",
        TRUE ~ "Suspect"
      )
    ) %>% 
    # MSF_visit_type
    mutate(
      type_of_visit = tolower(type_of_visit),
      MSF_visit_type = case_when(
        type_of_visit %in% "first_hosp" ~ "First hospitalisation",
        type_of_visit %in% "re-hosp" ~ "Rehospitalisation",
        type_of_visit %in% "first_hosp_after_other_cons" ~ "First hospitalisation after a consultation"
      )
    ) %>% 
    # MSF_refer_to
    mutate(
      MSF_refer_to = case_when(
        grepl("home", if_referred_for_care_isolation_hospital, ignore.case = TRUE) ~ NA_character_,
        TRUE ~ if_referred_for_care_isolation_hospital
      )
    ) %>% 
    # outcome_patcourse_status
    mutate(
      outcome = tolower(outcome),
      outcome_patcourse_status = case_when(
        outcome %in% "discharged - cured" ~ "Cured",
        outcome %in% "death" ~ "Died",
        outcome %in% "other" ~ "Other",
        outcome %in% "lama" ~ "Left against medical advice",
        outcome %in% "referral to hcf" ~ "Transferred",
        outcome %in% c("discharged - not a case", "discharged - isolation", "discharged - unknown") ~ "Sent back home" ### confirm with Vini
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
 


import_irq_ocb_ <- function(path, site) {
  
  readxl::read_xlsx(
    path, 
    skip = 3,
    col_types = "text",
    na = c("", "NA"),
    .name_repair = ~ vctrs::vec_as_names(..., repair = "unique", quiet = TRUE)
  ) %>% 
    janitor::clean_names() %>% 
    janitor::remove_empty("rows") %>% 
    mutate(linelist_row = 1:n(),
           upload_date = as.character(llutils::extract_date(path)),
           site = site)
}

