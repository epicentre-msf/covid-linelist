#' Import, standardize, and combine linelists from each facility
#'
#' @param path_data_raw Path to directory containing linelists
#' @param dict_linelist Master linelist variable dictionary
#'
#' @return
#' Combined linelist <tibble> created by binding together the most recent
#' linelist version for each facility, with minor cleaning (e.g. removing
#' almost-empty lines) and standardizing (e.g. variable names)
#' 
import_other_mmr_oca <- function(path_linelist_other, dict_linelist) {
  
  ## requires
  library(dplyr)
  library(hmatch)
  
  dict_facilities_join <- dict_facilities %>% 
    mutate_all(as.character) %>% 
    select(site, country, shape, OC, project, site_name, site_type, uid)
  
  path_to_files <- file.path(path_linelist_other, "OCA", "MMR")
  
  df_map <- file.path(path_to_files, "LL_v3.1_mapping_dhis2_Myitkyina.xlsx") %>% 
    readxl::read_xlsx() %>% 
    janitor::clean_names() %>% 
    select(1, 7, 8, 9, 10) %>% 
    setNames(c("var_epi", "map_type", "map_direct", "map_constant", "map_derive"))
  
  df_map_direct <- df_map %>% 
    filter(map_type == "1:1 correspondence") %>% 
    select(var_epi, map_direct) %>% 
    mutate(map_direct_std = hmatch::string_std(map_direct))
  
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
  
  files_ll <- llutils::list_files(
    path_to_files,
    pattern = "^LL_covid_dhis2_Myitkyina.*\\.xls",
    select = "latest"
  )
  
  d_orig <- readxl::read_xls(files_ll, col_types = "text") %>% 
    rename_with(hmatch::string_std) %>% 
    janitor::remove_empty("rows") %>% 
    mutate(
      linelist_row = 1:n(),
      upload_date = as.character(llutils::extract_date(files_ll)),
      site = "MMR_A_MYI"
    ) %>% 
    dplyr::left_join(dict_facilities_join, by = "site")
  
  
  ### Check for unseen values in derivation variables
  test_set_equal(
    d_orig$sex,
    c("[f] female", "[m] male", NA)
  )
  test_set_equal(
    d_orig$confirmation_status_at_exit,
    c(NA, "[sng] suspect (test negative)", "[pnd] probable (test not done)")
  )
  test_set_equal(
    d_orig$pregnant,
    c("[y] yes, currently pregnant", "[na] not applicable", "[n] no", NA) 
  )
  test_set_equal(
    d_orig$malaria_rdt_at_admission,
    c("[nd] not done", "[n] no", NA)
  )
  test_set_equal(
    d_orig$received_oxygen_therapy,
    c(NA, "0", "1") 
  )
  test_set_equal(
    d_orig$icu_admission,
    c(NA, "0", "1")
  )
  test_set_equal(
    d_orig$inpatient_exit_status,
    c(
      NA,
      "[ot] other",
      "[rf] external msf referral / transfer",
      "[la] left against medical advice",
      "[dh] discharged home", "[dd] dead"
    )
  )
  test_set_equal(
    d_orig$nutrition_status_at_admission,
    c("[mam] moderate acute malnutrition", "[nm] not malnourished", NA)
  )
  
  
  # df_map_direct %>% 
  #   filter(var_epi %in% dict_factors$variable)
  # 
  # d_orig %>% 
  #   count(inpatient_exit_status)
  # 
  # dict_factors %>% 
  #   filter(variable == "outcome_patcourse_status") %>% 
  #   select(1:2) %>% 
  #   print(n = "all")
  
  ### Derived variables
  d_derive <- d_orig %>% 
    mutate(
      across(c(date_of_admission, date_of_exit), ~ as.character(lubridate::as_date(.x))),
      patinfo_sex = case_when(
        sex %in% "[F] Female" ~ "F",
        sex %in% "[M] Male" ~ "M",
      ),
      MSF_covid_status = case_when(
        confirmation_status_at_exit %in% "[PND] Probable (test not done)" ~ "Probable",
        confirmation_status_at_exit %in% "[SNG] Suspect (test negative)" ~ "Suspected",
      ),
      Comcond_preg = case_when(
        pregnant %in% "[N] No" ~ "No",
        pregnant %in% "[NA] Not applicable" ~ NA_character_,
        pregnant %in% "[Y] Yes, currently pregnant" ~ "Yes"
      ),
      MSF_malaria = case_when(
        malaria_rdt_at_admission %in% "[N] No" ~ "Negative",
        malaria_rdt_at_admission %in% "[ND] Not done" ~ "Not done"
      ),
      MSF_received_oxygen = case_when(
        received_oxygen_therapy %in% "0" ~ "No",
        received_oxygen_therapy %in% "1" ~ "Yes"
      ),
      MSF_outcome_received_oxygen = case_when(
        received_oxygen_therapy %in% "0" ~ "No",
        received_oxygen_therapy %in% "1" ~ "Yes"
      ),
      patcourse_icu = case_when(
        icu_admission %in% "0" ~ "No",
        icu_admission %in% "1" ~ "Yes"
      ),
      outcome_patcourse_icu = case_when(
        icu_admission %in% "0" ~ "No",
        icu_admission %in% "1" ~ "Yes"
      ),
      outcome_patcourse_status = case_when(
        inpatient_exit_status %in% "[DD] Dead" ~ "Died",
        inpatient_exit_status %in% "[DH] Discharged home" ~ "Cured",
        inpatient_exit_status %in% "[LA] Left against medical advice" ~ "Left against medical advice",
        inpatient_exit_status %in% "[OT] Other" ~ "Other",
        inpatient_exit_status %in% "[RF] External MSF referral / transfer" ~ "Transferred"
      ),
      MSF_malnutrition = case_when(
        nutrition_status_at_admission %in% "[MAM] Moderate acute malnutrition" ~ "Yes",
        nutrition_status_at_admission %in% "[NM] Not malnourished" ~ "No",
      )
      # MSF_complications = case_when(
      #   ards %in% "1" ~ "Acute respiratory distress syndrome (ARDS)",
      #   renal_failure %in% "1" ~ "Renal distress",
      #   # cardiac_complications %in% "1" ~ "Heart failure", #???
      #   shock %in% "1" ~ "Other", #???
      #   sepsis %in% "1" ~ "Sepsis"
      # )
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
    mutate(
      patient_id = paste(site, format_text(MSF_N_Patient), sep = "_"),
      db_row = 1:n(),
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
 