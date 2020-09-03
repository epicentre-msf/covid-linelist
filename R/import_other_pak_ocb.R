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
import_other_pak_ocb <- function(path_linelist_other, dict_linelist) {
  
  ## requires
  library(dplyr)
  library(hmatch)
  source("R/import_other_pak_ocb.R")
  
  
  dict_facilities_join <- dict_facilities %>% 
    mutate_all(as.character) %>% 
    select(site, country, shape, OC, project, site_name, site_type, uid)

  path_to_files <- file.path(path_linelist_other, "OCB", "PAK")
  
  df_map <- file.path(path_to_files, "LL_v2.1_mapping_template_OCB_Timurgara.xlsx") %>% 
    readxl::read_xlsx() %>% 
    janitor::clean_names() %>% 
    select(1, 7, 8, 9, 10) %>% 
    setNames(c("var_epi", "map_type", "map_direct", "map_constant", "map_derive"))
  
  df_map_direct <- df_map %>% 
    filter(map_type == "1:1 correspondence") %>% 
    select(var_epi, map_direct) %>% 
    mutate(map_direct_std = janitor::make_clean_names(map_direct))
  
  vec_map_direct <- setNames(df_map_direct$map_direct_std, df_map_direct$var_epi)
  
  df_map_constant <- df_map %>% 
    filter(map_type == "Constant value") %>% 
    select(var_epi, map_constant) %>% 
    tidyr::pivot_wider(names_from = "var_epi", values_from = "map_constant")
  
  df_map_derive <- df_map %>% 
    filter(map_type == "Requires derivation") %>% 
    select(var_epi, map_derive)
  
  map_comcond <- readxl::read_xlsx(file.path(path_to_files, "Map_Comcond_OCB_Timurgara.xlsx")) %>% 
    select(-n)
  
  files_ll <- c(
    PAK_A_TIM = llutils::list_files(
      path_to_files,
      pattern = "COVID-19 Zero.*\\.xlsx",
      select = "latest"
    )
  )
  
  d_orig <- purrr::map2_dfr(
    files_ll,
    names(files_ll),
    import_pak_ocb_
  ) %>% 
    dplyr::left_join(dict_facilities_join, by = "site")
  

  
  ### Check for unseen values in derivation variables
  test_set_equal(d_orig$fever_y_n, c("yes", "no", "y", "n", NA))
  test_set_equal(d_orig$cough_y_n, c("yes", "no", "y", "n", NA))
  test_set_equal(d_orig$shortness_of_breath_y_n, c("yes", "no", "y", "n", NA))
  test_set_equal(d_orig$sore_throat_y_n, c("yes", "no", "y", "n", NA))
  test_set_equal(d_orig$headache_y_n, c("yes", "no", "y", "n", NA))
  test_set_equal(d_orig$flu, c("yes", "no", "y", "n", NA))
  test_set_equal(d_orig$close_contacts_of_propbable_confirmed_cases_yes_no_unknown, c("yes", "no", "unknown", "unkonwn", NA))
  test_set_equal(d_orig$first_lab_results, c("positive", "negative", "inconclusive", ".", NA))
  test_set_equal(
    d_orig$type_of_admission,
    c(NA, "died in consultation room", "transfer to dhq emergency", "home isolation",
      "referred for advance care", "dhq qurantine", "dhq isolation", "home qurantine")
  )
  test_set_equal(
    d_orig$outcome,
    c("deceased", "discharged for home isolation", "discharged for home quarentine", "defaulted", "tested negative",
      "referred for advanced care", "discharged cured", NA, "quarantine discontinued (contact neg result)", "isolation discontinued (neg result)") 
  )
  
  
  ### Derived variables
  d_derive <- d_orig %>% 
    # any symptoms?
    mutate(
      any_symp = pmap_lgl(
        list(fever_y_n, cough_y_n, shortness_of_breath_y_n, sore_throat_y_n, headache_y_n, flu),
        ~ any(grepl("^y", .x, ignore.case = TRUE))
      ),
      any_symp_no = pmap_lgl(
        list(fever_y_n, cough_y_n, shortness_of_breath_y_n, sore_throat_y_n, headache_y_n, flu),
        ~ any(grepl("^n", .x, ignore.case = TRUE))
      )
    ) %>% 
    # MSF_covid_status
    mutate(close_contact = close_contacts_of_propbable_confirmed_cases_yes_no_unknown) %>% 
    mutate(
      MSF_covid_status = case_when(
        tolower(first_lab_results) == "positive" ~ "Confirmed",
        tolower(first_lab_results) == "negative" ~ "Not a case",
        !tolower(first_lab_results) %in% c("positive", "negative") & any_symp & tolower(close_contact) == "yes" ~ "Probable",
        !tolower(first_lab_results) %in% c("positive", "negative") & (any_symp | tolower(close_contact) == "yes") ~ "Suspected",
        !tolower(first_lab_results) %in% c("positive", "negative") & any_symp_no & tolower(close_contact) == "no" ~ "Not a suspect"
      )
    ) %>% 
    # Comcond variables
    mutate(comcond = toupper(co_morbidity_htn_diabetes_asthma_etc)) %>% 
    left_join(map_comcond, by = "comcond") %>% 
    # MSF_visit_type
    mutate(
      MSF_visit_type = case_when(
        tolower(type_of_admission) %in% c("dhq isolation", "dhq qurantine") ~ "First hospitalization",
        tolower(type_of_admission) %in% c("home isolation", "home qurantine") ~ "First consultation",
        tolower(type_of_admission) %in% c("referred for advance care", "died in consultation room") ~ "First consultation",
        tolower(type_of_admission) %in% c("died in er", "transfer to dhq emergency") ~ "First consultation"
      )
    ) %>% 
    # MSF_date_consultation
    mutate(
      date_adm_covid = as.integer(date_of_admission_to_covid_service),
      date_adm_triag = as.integer(date_of_admission_arrival_in_the_pre_triage_er_mch),
      MSF_date_consultation = map2_chr(date_adm_covid, date_adm_triag, ~ as.character(min_safe(c(.x, .y))))
    ) %>% 
    # outcome_patcourse_status
    mutate(
      outcome_patcourse_status = case_when(
        tolower(outcome) == "deceased" ~ "Died",
        tolower(outcome) == "defaulted" ~ "Left against medical advice",
        tolower(outcome) == "discharged cured" ~ "Cured",
        tolower(outcome) == "discharged for home isolation" ~ "Sent back home",
        tolower(outcome) == "discharged for home quarentine" ~ "Sent back home",
        tolower(outcome) == "isolation discontinued (neg result)" ~ "Cured",
        tolower(outcome) == "quarantine discontinued (contact neg result)" ~ "Cured",
        tolower(outcome) == "referred for advanced care" ~ "Transferred",
        tolower(outcome) == "tested negative" ~ "Cured",
        tolower(outcome) == "14 day quarantine completed" ~ "Cured"
      )
    ) %>% 
    # MSF_treament1
    # if one of Chloroquine and Azithromcyin, put Chloroquine treatment1 and Azithromcyin treatment2
    # else if only one put treatment1
    mutate(
      MSF_treament1 = case_when(
        tolower(chloroquine_treatment) %in% "yes" ~ "Chloroquine",
        !tolower(chloroquine_treatment) %in% "yes" & tolower(azithromcyin_treatment) %in% "yes" ~ "Azithromcyin"
      ),
      MSF_treament2 = case_when(
        tolower(chloroquine_treatment) %in% "yes" & tolower(azithromcyin_treatment) %in% "yes" ~ "Azithromcyin",
      )
    )
  
  
  ### Constants and 1:1 mappings
  d_out <- d_derive %>% 
    map_columns(., vec_map_direct) %>% 
    select(-any_of(names(df_map_constant))) %>%
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
 



import_pak_ocb_ <- function(path, site) {
  
  readxl::read_xlsx(
    path, 
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

