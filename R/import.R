#' Import, standardize, and combine linelists from each facility
#'
#' @param country Country ISO code
#' @param path_data_raw Path to directory containing linelists
#' @param dict_facilities Dictionary mapping site-ID columns (country, OC,
#'   project) to site codes
#' @param dict_linelist Main linelist dictionary
#' @param dict_extra_vars Dictionary for renaming additional variables
#' @param dict_vars_exclude Dictionary of empty variables exported from v2
#'
#' @return
#' Combined linelist <tibble> created by binding together the most recent
#' linelist version for each facility, with minor cleaning (e.g. removing
#' almost-empty lines) and standardizing (e.g. variable names)
#' 
import_linelists <- function(country,
                             path_data_raw,
                             dict_facilities,
                             dict_linelist,
                             dict_extra_vars,
                             dict_vars_exclude) {
  
  ## requires
  library(dplyr)
  library(hmatch)
  source("R/import.R")
  
  ## scan and parse linelist files to identify the most recent linelist file to
  # import for each facility
  df_sheets <- scan_sheets(path_data_raw, country, dict_facilities)
  
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
  df_data <- df_sheets %>%
    group_by(country, shape, OC, project, site_type, site_name, site, uid, upload_date) %>% 
    do(read_and_prepare_data(file_path = .$file_path, dict_extra_vars = dict_extra_vars)) %>%
    ungroup() %>% 
    mutate(patient_id = paste(site, format_text(MSF_N_Patient), sep = "_")) %>% 
    mutate(db_row = 1:n())
  
  
  ## columns to add (from original ll template)
  ll_template <- dict_linelist$code_name
  ll_template <- ll_template[!grepl("^MSF_variable_additional", ll_template)]
  cols_to_add <- setdiff(ll_template, names(df_data))
  df_data[cols_to_add] <- NA_character_
  
  # check for new columns to be manually renamed
  extra_cols <- grep("^extra__", names(df_data), value = TRUE)
  new_cols <- setdiff(names(df_data), c(cols_derive, ll_template, extra_cols))
  
  # remove unnecessary columns
  pref_exclude <- "^X0|^Añadir.el.nombre|^Ajouter.le.nom|^Add.variable.name|^MSF_variable_additional"
  new_cols <- new_cols[!grepl(pref_exclude, new_cols)]
  new_cols <- setdiff(new_cols, dict_vars_exclude[[1]])
  
  # arrange cols
  if (length(new_cols) > 0) {
    message("New linelist columns to rename:\n", paste(new_cols, collapse = "\n"))
  }
  
  ## return
  dplyr::select(df_data, all_of(cols_derive), all_of(ll_template), starts_with("extra_"))
}



#' Scan and parse linelist filenames to identify files to import
#'
#' @param path_data_raw Path to directory containing linelists
#' @param country Country ISO code
#' @param dict_facilities Dictionary with facility-level metadata
#' @param return_latest Logical indicating whether to return only the most
#'   recent linelist by site
#'
#' @return
#' A tibble with one row per facility, with columns identifying the most
#' recent linelist file to import, including file_path, upload_date, etc.
#' 
scan_sheets <- function(path_data_raw,
                        country,
                        dict_facilities,
                        return_latest = TRUE) {

  ## requires
  library(dplyr)
  library(tidyr)
  library(glue)
  
  # paths to linelist files for given country
  path_data_raw_country <- file.path(path_data_raw, country)
  files_country <- list.files(path_data_raw_country,
                              pattern = glue::glue("^linelist_Covid_anonymous__{country}"))

  # regex patterns to remove from file path prior to parsing
  reg_rm <- "^linelist_Covid_anonymous__|_[[:digit:]]{2}-[[:digit:]]{2}\\.xlsb"

  # variables to parse from file path
  vars_parse <- c("country",
                  "OC",
                  "project",
                  "site_type",
                  "site_name",
                  "key",
                  "upload_date")
  
  ## prep dict_facilities for join
  dict_facilities_join <- dict_facilities %>% 
    mutate_all(as.character) %>% 
    mutate(site_name_join = hmatch::string_std(site_filename)) %>% 
    select(site, country, shape, OC, project, site_name, uid,  site_name_join)
  
  # parse files and retain only most recent file by site
  df_sheet <- tibble::tibble(file_path = files_country) %>%
    mutate(path_parse = gsub(reg_rm, "", file_path)) %>% 
    mutate(path_parse = gsub("lon_.+_(?=2020)", "0000__", path_parse, perl = TRUE)) %>% 
    tidyr::separate(path_parse, vars_parse, sep = "_{2}") %>% 
    mutate_all(~ ifelse(.x == "", NA_character_, .x)) %>% 
    mutate(site_name_join = hmatch::string_std(site_name)) %>% 
    select(-site_name, -project, -key) %>% 
    left_join(dict_facilities_join, by = c("country", "OC", "site_name_join")) %>% 
    select(-site_name_join) %>% 
    mutate(file_path = file.path(path_data_raw_country, file_path))
  
  if (return_latest) {
    df_sheet <- df_sheet %>% 
      group_by(site) %>%
      arrange(desc(upload_date)) %>% 
      slice(1) %>%
      ungroup()
  }
 
  ## return
  df_sheet
}



#' Read and prepare individual linelist file
#'
#' @param file_path Path to linelist
#' @param dict_extra_vars Dictionary defining standardized column names for
#'   MSF_extra_* variables
#'
#' @return
#' Linelist <tibble> for a single facility, after minor cleaning (e.g. removing
#' almost-empty lines) and standardizing (e.g. variable names)
#'
read_and_prepare_data <- function(file_path, dict_extra_vars) {

  ## requires
  library(dplyr)
  library(janitor)
  library(readxlsb)
  source("R/utilities.R")
  
  ## read linelist from relevant excel sheet
  df <- readxlsb::read_xlsb(file_path,
                            sheet = "linelist",
                            col_types = "string")
  
  ## get linelist version and language
  df_meta <- get_site_meta(file_path)
  
  if (nrow(df) > 0) {
    ## check for column names beginning with ".." (missing name in excel file)
    if (any(grepl("^\\.\\.", names(df)))) {
      stop("Missing column names in file: ", file_path)
    }
    
    ## remove empty rows/cols, recode column names, and add linelist row number
    out <- df %>% 
      dplyr::as_tibble() %>% 
      mutate_all(~ ifelse(.x == "", NA_character_, .x)) %>% 
      janitor::remove_empty(which = c("rows")) %>%
      dplyr::rename_with(., .fn = recode_columns, dict_extra_vars = dict_extra_vars) %>%
      mutate(linelist_row = seq_len(n())) %>% 
      select(linelist_row, everything()) %>% 
      bind_cols(df_meta, .)
    
  } else {
    out <- tibble(linelist_row = integer(0))
  }

  out
}



recode_columns <- function(x, dict_extra_vars) {
  
  # requires
  library(dplyr)
  library(hmatch)
  
  # standardize extra names dict
  dict_extra_vars_prep <- dict_extra_vars %>% 
    mutate(match = hmatch::string_std(name_raw))  
  
  out <- data.frame(orig = x) %>% 
    mutate(match = hmatch::string_std(orig)) %>% 
    left_join(dict_extra_vars_prep, by = "match") %>% 
    mutate(orig = ifelse(!is.na(name), name, orig))
  
  return(out$orig)
}

