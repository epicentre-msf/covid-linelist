#' Import, standardize, and combine linelists from each facility
#'
#' @param path_data_raw Path to directory containing linelists
#' @param country ISO country code
#' @param ll_template 
#' @param dict_facilities 
#'
#' @return
#' Combined linelist <tibble> created by binding together the most recent
#' linelist version for each facility, with minor cleaning (e.g. removing
#' almost-empty lines) and standardizing (e.g. variable names)
#' 
import_linelists <- function(path_data_raw,
                             country,
                             ll_template,
                             dict_facilities) {
  
  ## requires
  library(dplyr)
  source("R/import.R")
  
  ## scan and parse linelist files to identify the most recent linelist file to
  # import for each facility
  df_sheets <- scan_sheets(path_data_raw, country)
  
  ## derived columns
  cols_derive <- c("db_row",
                   "linelist_row",
                   "upload_date",
                   "country",
                   "OC",
                   "project",
                   "site_type",
                   "site_name",
                   "site",
                   "MSF_N_Patient",
                   "patient_id")
  
  ## import and prepare
  df_data <- df_sheets %>%
    # TODO: improve this derivation of site via left_join below
    left_join(dict_facilities, by = c("country", "OC", "project")) %>% 
    group_by(country, OC, project, site_type, site_name, site, upload_date) %>% 
    do(read_and_prepare_data(file_path = .$file_path)) %>%
    ungroup() %>% 
    mutate(patient_id = paste(site, MSF_N_Patient, sep = "_")) %>% 
    mutate(db_row = 1:n())
  
  ## columns to add
  cols_to_add <- setdiff(ll_template, names(df_data))
  df_data[cols_to_add] <- NA_character_
  
  ## return
  dplyr::select(df_data, all_of(cols_derive), all_of(ll_template))
}



#' Scan and parse linelist filenames to identify files to import
#'
#' @param path_data_raw Path to directory containing linelists
#' @param country ISO country code
#'
#' @return
#' A tibble with one row per facility, with columns identifying the most
#' recent linelist file to import, including file_path, upload_date, etc.
#' 
scan_sheets <- function(path_data_raw, country) {

  ## requires
  library(dplyr)
  library(tidyr)
  
  # paths to linelist files for given country
  path_data_raw_country <- file.path(path_data_raw, country)
  files_country <- list.files(path_data_raw_country, pattern = "^linelist_Covid_")

  # regex patterns to remove from file path prior to parsing
  reg_rm <- "^linelist_Covid_anonymous__|_[[:digit:]]{2}-[[:digit:]]{2}\\.xlsb"
  
  # variables to parse from file path
  vars_parse <- c("country",
                  "OC",
                  "project",
                  "site_type",
                  "site_name",
                  "site_longitude",
                  "site_latitude",
                  "upload_date")
  
  # parse files and retain only most recent file by site
  df_sheet <- tibble::tibble(file_path = files_country) %>%
    mutate(path_parse = gsub(reg_rm, "", file_path)) %>% 
    tidyr::separate(path_parse, vars_parse, sep = "_{1,3}") %>% 
    group_by(country, OC, project) %>%
    filter(upload_date == max(upload_date, na.rm = TRUE)) %>% 
    filter(file_path == max(file_path)) %>%  # avoid multi-export duplicates
    ungroup() %>% 
    mutate(file_path = file.path(path_data_raw_country, file_path))
 
  ## return
  df_sheet
}



#' Read and prepare individual linelist file
#'
#' @param file_path Path to linelist
#'
#' @return
#' Linelist <tibble> for a single facility, after minor cleaning (e.g. removing
#' almost-empty lines) and standardizing (e.g. variable names)
#'
read_and_prepare_data <- function(file_path) {

  ## requires
  library(dplyr)
  library(janitor)
  library(readxlsb)
  
  ## read linelist from relevant excel sheet
  df <- readxlsb::read_xlsb(file_path,
                            sheet = 1,
                            col_types = "string")

  ## check for column names beginning with ".." (missing name in excel file)
  if (any(stringr::str_detect(names(df), "^\\.\\."))) {
    stop("Missing column names in file: ", file_path)
  }

  ## remove empty rows/cols, recode column names, and add linelist row number
  df %>% 
    dplyr::as_tibble() %>% 
    mutate_all(~ ifelse(.x == "", NA_character_, .x)) %>% 
    janitor::remove_empty(which = c("rows")) %>%
    mutate(linelist_row = 1:n()) %>% 
    select(linelist_row, everything())
}

