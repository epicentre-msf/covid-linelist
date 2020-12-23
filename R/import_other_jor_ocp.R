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
import_other_jor_ocp <- function(path_linelist_other, dict_linelist) {
  
  ## requires
  library(dplyr)
  library(hmatch)
  source("R/import_other_jor_ocp.R")
  
  dict_facilities_join <- dict_facilities %>% 
    mutate_all(as.character) %>% 
    select(site, country, shape, OC, project, site_name, site_type, uid)

  path_to_files <- file.path(path_linelist_other, "OCP", "JOR")
  
  df_map <- file.path(path_to_files, "LL_v3.0_Mapping_Template_Amman.xlsx") %>% 
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
    JOR_P_RSP = llutils::list_files(
      path_to_files,
      pattern = "^Amman.*\\.xlsx",
      select = "latest"
    )
  )
  
  d_orig <- purrr::map2_dfr(
    files_ll,
    names(files_ll),
    import_jor_ocp_
  ) %>% 
    select(-any_of(c("country", "OC", "project", "site_type", "site_name"))) %>% 
    left_join(dict_facilities, by = "site")
  
  ### Check for unseen values in derivation variables
  test_set_equal(d_orig$msf_symptom_muscle_aches, c("yes", "no", "unknown", NA))
  test_set_equal(d_orig$msf_symptom_joint_pain, c("yes", "no", "unknown", NA))
  test_set_equal(d_orig$comcond_chronic_lung_disease, c("yes", "no", "unknown", NA))
  test_set_equal(d_orig$comcond_asthma, c("yes", "no", "unknown", NA))
  # test_set_equal(d_orig$msf_symptom_asymptomatic, c("yes", "no", "unknown", NA))
  
  ### Derived variables
  d_derive <- d_orig %>% 
    # MSF_symptom_aches, Comcond_lung, MSF_type_lung_disease
    mutate(
      # patcourse_asymp = case_when(
      #   tolower(msf_symptom_asymptomatic) == "yes" ~ "No",
      #   tolower(msf_symptom_asymptomatic) == "no" ~ "Yes",
      #   TRUE ~ msf_symptom_asymptomatic
      # ),
      MSF_symptom_aches = merge_vars(msf_symptom_muscle_aches, msf_symptom_joint_pain),
      Comcond_lung = merge_vars(comcond_chronic_lung_disease, comcond_asthma),
      MSF_type_lung_disease = if_else(tolower(comcond_asthma) == "yes", "Asthma", NA_character_)
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


import_jor_ocp_ <- function(path, site) {
  
  readxl::read_xlsx(
    path, 
    col_types = "text",
    na = c("", "NA"),
    .name_repair = ~ vctrs::vec_as_names(..., repair = "unique", quiet = TRUE)
  ) %>% 
    janitor::clean_names() %>% 
    janitor::remove_empty("rows") %>% 
    mutate(
      linelist_row = 1:n(),
      upload_date = as.character(llutils::extract_date(path)),
      site = site
    )
}

