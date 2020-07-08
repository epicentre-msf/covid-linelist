#' Import, standardize, and combine linelists from each facility
#'
#' @param path_linelist_other
#' @param dict_facilities Dictionary mapping site-ID columns (country, OC,
#'   project) to site codes
#'
#' @return
#' Combined linelist <tibble> created by binding together the most recent
#' linelist version for each facility, with minor cleaning (e.g. removing
#' almost-empty lines) and standardizing (e.g. variable names)
#' 
import_other_yem_pra <- function(path_linelist_other, dict_linelist) {
  
  ## requires
  library(dplyr)
  library(hmatch)
  source("R/import_other_yem_pra.R")
  
  
  file_ll <- llu::list_files(
    file.path(path_linelist_other, "HIS OCP"),
    pattern = "^ocp_covid19_praxis.*\\.csv",
    ignore.case = TRUE,
    full.names = TRUE,
    select = "latest"
  )
  
  d_orig <- import_other_yem_pra_helper(
    file_ll,
    site = "YEM_P_ASA"
  )
  
  d_derived <- d_orig %>% 
    mutate(MSF_admin_location_past_week = adm2_name__res)
  
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
  df_data <- d_orig %>% 
    mutate(patient_id = paste(site, format_text(MSF_N_Patient), sep = "_")) %>% 
    mutate(db_row = 1:n())
  
  ## columns to add (from original ll template)
  ll_template <- dict_linelist$code_name
  cols_to_add <- setdiff(c(ll_template, cols_derive), names(df_data))
  df_data[cols_to_add] <- NA_character_
  
  # check for new columns to be manually renamed
  extra_cols <- grep("^extra__", names(df_data), value = TRUE)
  new_cols <- setdiff(names(df_data), c(cols_derive, ll_template, extra_cols))
  
  ## return
  dplyr::select(df_data, all_of(cols_derive), all_of(ll_template), starts_with("extra_"))
}
 



import_other_yem_pra_helper <- function(path, site) {
  
  suppressMessages(readr::read_csv(path)) %>% 
    mutate_all(as.character) %>% 
    rename(project = Project,
           country = Country,
           site_name = Site_name,
           site_type = Site_type,
           linelist_lang = ll_language,
           linelist_vers = ll_version,
           upload_date = upload_Date,
           MSF_test_results = MSF_test_first_results,
           MSF_visit_type = MSF_readmission) %>% 
    mutate(linelist_row = 1:n(),
           upload_date = stringr::str_extract(upload_date, "[0-9]{4}.?[0-9]{2}.?[0-9]{2}"),
           shape = "YEM",
           OC = "OCP",
           site = site)
}
