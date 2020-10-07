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
import_other_ind_ocb <- function(path_linelist_other, dict_linelist) {
  
  ## requires
  library(dplyr)
  library(hmatch)
  source("R/import_other_ind_ocb.R")
  
  dict_facilities_join <- dict_facilities %>% 
    mutate_all(as.character) %>% 
    select(site, country, shape, OC, project, site_name, site_type, uid)

  path_to_files <- file.path(path_linelist_other, "OCB", "IND")
  
  df_map <- file.path(path_to_files, "LL_v2.1_mapping_template_OCB_MumbaiSHFC.xlsx") %>% 
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
  
  files_ll <- c(
    IND_B_SHA = llutils::list_files(
      path_to_files,
      pattern = "^MumbaiSHFC_.*\\.xlsx",
      select = "latest"
    )
  )
  
  d_orig <- purrr::map2_dfr(
    files_ll,
    names(files_ll),
    import_ind_ocb_
  ) %>% 
    dplyr::left_join(dict_facilities_join, by = "site")
  
  # ### Check for unseen values in derivation variables
  test_set_equal(d_orig$is_this_patient_a_co_vi_d_19_suspect, c("yes", "no", NA))
  test_set_equal(d_orig$patient_covid19_laboratory_report, c("indeterminate", "positive", "negative", "not available", NA))
  test_set_equal(d_orig$x1_fever_in_the_last_10_days, c("yes", "no", "don't know", NA))
  test_set_equal(d_orig$x4_fever_100_4_f_38_degree_c, c("yes", "no", NA))
  test_set_equal(d_orig$hiv, c("yes", "no", NA))
  test_set_equal(d_orig$on_immunosuppressant_drugs, c("yes", "no", NA))
  test_set_equal(d_orig$tuberculosis, c("yes", "no", NA))
  test_set_equal(d_orig$visit_type, c("others", "second", "first", NA))
  test_set_equal(
    d_orig$if_patient_is_a_co_vi_d_19_suspect_then_patient_referred_to,
    c("referred to the dch", "others (please specify)", "referred to the dchc", "referred to the ccc", NA)
  )
  
  # d_derive %>%
  #   count(is_this_patient_a_co_vi_d_19_suspect, patient_covid19_laboratory_report, MSF_covid_status)
  # 
  # d_derive %>%
  #   count(patient_covid19_laboratory_report, MSF_test_type)
  # 
  # d_derive %>%
  #   count(x1_fever_in_the_last_10_days, x4_fever_100_4_f_38_degree_c, MSF_symptom_fever)
  # 
  # d_derive %>%
  #   count(hiv, on_immunosuppressant_drugs, Comcond_immuno)
  # 
  # d_derive %>%
  #   count(tuberculosis, MSF_tb_active)
  # 
  # d_derive %>%
  #   count(is_this_patient_a_co_vi_d_19_suspect,
  #         if_patient_is_a_co_vi_d_19_suspect_then_patient_referred_to,
  #         # name_of_the_ccc,
  #         # name_of_the_dch,
  #         # name_of_the_dchc,
  #         # name_of_the_other_centre,
  #         outcome_patcourse_status)
  
  ### Derived variables
  d_derive <- d_orig %>% 
    # MSF_covid_status
    mutate(
      patient_covid19_laboratory_report = if_else(patient_covid19_laboratory_report == "not available", NA_character_, patient_covid19_laboratory_report),
      across(c(is_this_patient_a_co_vi_d_19_suspect, patient_covid19_laboratory_report), tolower),
      MSF_covid_status = case_when(
        is_this_patient_a_co_vi_d_19_suspect == "no" ~ "Not a suspect",
        is_this_patient_a_co_vi_d_19_suspect %in% "yes" & is.na(patient_covid19_laboratory_report) ~ "Suspected",
        patient_covid19_laboratory_report %in% "positive" ~ "Confirmed",
        patient_covid19_laboratory_report %in% "indeterminate" ~ "Probable",
        patient_covid19_laboratory_report %in% "negative" ~ "Not a case"
      )
    ) %>% 
    # MSF_test_type
    mutate(
      MSF_test_type = case_when(
        patient_covid19_laboratory_report %in% c("positive", "negative", "indeterminate") ~ "PCR",
        TRUE ~ NA_character_
      )
    ) %>% 
    # MSF_visit_type
    mutate(
      visit_type = tolower(visit_type),
      MSF_visit_type = case_when(
        visit_type %in% "first" ~ "First consultation",
        visit_type %in% "second" ~ "Other",
        visit_type %in% "others" ~ "Other"
      )
    ) %>% 
    # MSF_symptom_fever
    mutate(
      across(c(x1_fever_in_the_last_10_days, x4_fever_100_4_f_38_degree_c), tolower),
      MSF_symptom_fever = case_when(
        x1_fever_in_the_last_10_days %in% "yes" | x4_fever_100_4_f_38_degree_c %in% "yes" ~ "Yes",
        x1_fever_in_the_last_10_days %in% "no" & x4_fever_100_4_f_38_degree_c %in% "no" ~ "No",
        is.na(x1_fever_in_the_last_10_days) & is.na(x4_fever_100_4_f_38_degree_c) ~ NA_character_,
        TRUE ~ "Unknown"
      )
    ) %>% 
    # Comcond_immuno
    mutate(
      across(c(hiv, on_immunosuppressant_drugs), tolower),
      Comcond_immuno = case_when(
        hiv %in% "yes" | on_immunosuppressant_drugs %in% "yes" ~ "Yes",
        hiv %in% "no" & on_immunosuppressant_drugs %in% "no" ~ "No",
        is.na(hiv) & is.na(on_immunosuppressant_drugs) ~ NA_character_,
        TRUE ~ "Unknown"
      )
    ) %>% 
    # MSF_tb_active
    mutate(
      across(c(tuberculosis), tolower),
      MSF_tb_active = case_when(
        tuberculosis %in% "yes" ~ "Yes (unknown)"
      )
    ) %>% 
    # outcome_patcourse_status
    mutate(
      referral = tolower(if_patient_is_a_co_vi_d_19_suspect_then_patient_referred_to),
      outcome_patcourse_status = case_when(
        referral %in% c("referred to the dch", "referred to the dchc", "referred to the ccc") ~ "Transferred",
        referral %in% c("others (please specify)") ~ "Sent back home"
      )
    ) %>% 
    # MSF_refer_to
    mutate(
      MSF_refer_to = pmap_chr(list(name_of_the_ccc, name_of_the_dch, name_of_the_dchc), collapse_name)
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
 

import_ind_ocb_ <- function(path, site) {
  
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

