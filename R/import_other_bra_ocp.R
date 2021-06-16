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
import_other_bra_ocp <- function(path_linelist_other, dict_linelist) {
  
  ## requires
  library(dplyr)
  library(hmatch)
  
  dict_facilities_join <- dict_facilities %>% 
    mutate_all(as.character) %>% 
    select(site, country, shape, OC, project, site_name, site_type, uid)
  
  path_to_files <- file.path(path_linelist_other, "OCP", "BRA")
  
  df_map <- file.path(path_to_files, "LL_v3.0_mapping_template_Jiparana.xlsx") %>% 
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
  
  files_ll <- llutils::list_files(
    path_to_files,
    pattern = "jiparana_msf.*\\.csv",
    select = "latest"
  )
  
  d_orig <- readr::read_csv(files_ll, col_types = cols(.default = col_character())) %>% 
    janitor::clean_names() %>% 
    janitor::remove_empty("rows") %>% 
    mutate(
      linelist_row = 1:n(),
      upload_date = as.character(llutils::extract_date(files_ll)),
      site = "BRA_P_JIP"
    ) %>% 
    dplyr::left_join(dict_facilities_join, by = "site")
  
  
  ### Derived variables
  d_derive <- d_orig %>% 
    mutate(
      MSF_N_Patient = id,
      MSF_date_consultation = as.character(lubridate::as_date(admitdt, format = "%d%b%Y")),
      outcome_date_of_outcome = as.character(lubridate::as_date(exitdt, format = "%d%b%Y")),
      outcome_patcourse_status = case_when(
        outcome == "AMA" ~ "Left against medical advice",
        outcome == "disch" ~ "Cured",
        outcome == "referral" ~ "Transferred"
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
 
