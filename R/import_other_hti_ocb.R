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
import_other_hti_ocb <- function(path_linelist_other, dict_linelist) {
  
  ## requires
  library(dplyr)
  library(hmatch)
  
  path_to_files <- file.path(path_linelist_other, "OCB", "HTI")
  
  df_map <- file.path(path_to_files, "LL_v2.1_mapping_template_OCB_HTI.xlsx") %>% 
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
  
  file_ll <- llu::list_files(
    path_to_files,
    pattern = "BD_LL_MT.*\\.xlsx",
    ignore.case = TRUE,
    full.names = TRUE,
    select = "latest"
  )
  
  dict_facilities_join <- dict_facilities %>% 
    mutate_all(as.character) %>% 
    select(site, country, shape, OC, project, site_name, site_type, uid)
  
  dict_facilities_join %>% 
    filter(site == "HTI_B_MAR")
  
  d_orig <- readxl::read_xlsx(file_ll, col_types = "text") %>% 
    janitor::clean_names() %>% 
    mutate(site = "HTI_B_MAR") %>% 
    dplyr::left_join(dict_facilities_join, by = "site") %>% 
    mutate(upload_date = as.character(llu::extract_date(file_ll)))
  
  
  ### Check for unseen values in derivation variables
  test_set_equal(d_orig$symptomatique, c("oui", "non", NA))
  test_set_equal(d_orig$symptomatique_a_la_sortie,  c("oui", "non", "na, sympt. à l'admission", NA))
  
  ### Derived variables
  d_derive <- d_orig %>% 
    mutate(MSF_N_Patient = paste0("TEMPID_", formatC(1:n(), width = 3, flag = "0"))) %>% 
    mutate(across(c(symptomatique, symptomatique_a_la_sortie), tolower)) %>% 
    mutate(patcourse_asymp = case_when(
      symptomatique == "oui" ~ "Non",
      symptomatique == "non" ~ "Oui"
    )) %>% 
    mutate(outcome_asymp = case_when(
      symptomatique == "oui" | symptomatique_a_la_sortie == "oui" ~ "Non",
      !symptomatique %in% "oui" & symptomatique_a_la_sortie == "no" ~ "Oui"
    ))
  
  # d_derive %>%
  #   count(symptomatique, symptomatique_a_la_sortie, patcourse_asymp, outcome_asymp)
    
  
  ### Constants and 1:1 mappings
  d_out <- d_derive %>% 
    rename(all_of(vec_map_direct)) %>% 
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
 

