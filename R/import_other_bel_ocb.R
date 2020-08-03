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
import_other_bel_ocb <- function(path_linelist_other, dict_linelist) {
  
  ## requires
  library(dplyr)
  library(hmatch)
  
  path_to_files <- file.path(path_linelist_other, "OCB", "BEL")
  
  df_map <- file.path(path_to_files, "LL_v2.1_mapping_template_OCB_BEL.xlsx") %>% 
    readxl::read_xlsx() %>% 
    janitor::clean_names() %>% 
    select(1, 6, 7, 8, 9, 10) %>% 
    setNames(c("var_epi", "category", "map_type", "map_direct", "map_constant", "map_derive"))
  
  df_map_direct <- df_map %>% 
    filter(map_type == "1:1 correspondence") %>% 
    select(var_epi, map_direct) %>% 
    mutate(map_direct_std = janitor::make_clean_names(map_direct))
  
  vec_map_direct <- setNames(df_map_direct$map_direct_std, df_map_direct$var_epi)
  
  df_map_constant <- df_map %>% 
    filter(map_type == "Constant value", !category %in% "Site metadata") %>% 
    select(var_epi, map_constant) %>% 
    tidyr::pivot_wider(names_from = "var_epi", values_from = "map_constant")
  
  df_map_derive <- df_map %>% 
    filter(map_type == "Requires derivation") %>% 
    select(var_epi, map_derive)
  
  file_ll <- llutils::list_files(
    path_to_files,
    pattern = "TT linelist.*\\.xlsx",
    ignore.case = TRUE,
    full.names = TRUE,
    select = "latest"
  )
  
  dict_facilities_join <- dict_facilities %>% 
    mutate_all(as.character) %>% 
    select(site, country, shape, OC, project, site_name, site_type, uid)
  
  d_orig <- readxl::read_xlsx(file_ll, col_types = "text", skip = 1) %>% 
    janitor::remove_empty("rows") %>% 
    janitor::clean_names() %>% 
    mutate(site = "BEL_B_TOU") %>% 
    dplyr::left_join(dict_facilities_join, by = "site") %>% 
    mutate(upload_date = as.character(llutils::extract_date(file_ll)))

  ### Check for unseen values in derivation variables
  test_set_equal(d_orig$substance_abuse, c("yes", "no", NA))
  test_set_equal(d_orig$gastro_intestinal, c("yes", "no", NA))
  test_set_equal(d_orig$psychiatric_condition, c("yes", "no", NA))
  
  ### Derived variables
  d_derive <- d_orig %>% 
    # derive Comcond_other
    mutate(across(c(substance_abuse, gastro_intestinal, psychiatric_condition), tolower)) %>% 
    mutate(substance_abuse = ifelse(substance_abuse == "yes", "Substance abuse", NA),
           gastro_intestinal = ifelse(gastro_intestinal == "yes", "Gastrointestinal", NA),
           psychiatric_condition = ifelse(psychiatric_condition == "yes", "Psychiatric condition", NA)) %>% 
    unite(Comcond_other, c(substance_abuse, gastro_intestinal, psychiatric_condition, other_63), na.rm = TRUE, sep = ", ", remove = FALSE) %>% 
    mutate(Comcond_other = ifelse(Comcond_other == "", NA_character_, Comcond_other)) %>% 
    # derive MSF_refer_from
    unite(MSF_refer_from, c(referral_source_type, referral_source_name), na.rm = TRUE, sep = ": ", remove = FALSE) %>% 
    mutate(MSF_refer_from = ifelse(MSF_refer_from == "", NA_character_, MSF_refer_from)) %>% 
    # derive MSF_date_consultation
    mutate(across(c(date_of_admission_to_suspect_ward, date_of_admission_to_confirmed_ward), parse_dates)) %>% 
    mutate(MSF_date_consultation = map2_chr(date_of_admission_to_suspect_ward, date_of_admission_to_confirmed_ward,
                                            ~ as.character(min_safe(c(.x, .y))))) %>%
    # derive copies of MSF_date_consultation
    mutate(patcourse_presHCF = MSF_date_consultation,
           outcome_patcourse_presHCF = MSF_date_consultation)
  
    # d_derive %>% 
    #   select(Comcond_other, substance_abuse, gastro_intestinal, psychiatric_condition, other_63) %>% 
    #   unique()  
    
    # d_derive %>%
    #   count(MSF_refer_from, referral_source_type, referral_source_name) %>%
    #   print(n = "all")
  
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
    mutate(linelist_lang = "English",
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
 

