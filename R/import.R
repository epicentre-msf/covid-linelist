#' Import, standardize, and combine linelists from each facility
#'
#' @param country Country ISO code
#' @param path_data_raw Path to directory containing linelists
#' @param dict_facilities Dictionary mapping site-ID columns (country, OC,
#'   project) to site codes
#' @param dict_linelist Main linelist dictionary
#' @param dict_extra_vars Dictionary for renaming additional variables
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
                             site_exclude = NULL) {
  
  ## requires
  library(dplyr, warn.conflicts = FALSE)
  library(purrr, warn.conflicts = FALSE)
  library(tidyr, warn.conflicts = FALSE)
  library(hmatch, warn.conflicts = FALSE)
  source("R/import.R")
  
  if (FALSE) {
    country <- "AFG"
  }
  
  ## scan and parse linelist files to identify the most recent linelist file to
  # import for each facility
  df_sheets <- scan_sheets(path_data_raw, country, dict_facilities) %>% 
    filter(!site %in% site_exclude)
  
  if (nrow(df_sheets) == 0) return(NULL)
  
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
  df_data <- df_sheets %>% 
    mutate(
      df = purrr::map2(
        file_path,
        site,
        read_and_prepare_data,
        dict_linelist = dict_linelist,
        dict_extra_vars = dict_extra_vars,
        cols_derive = cols_derive
      )
    ) %>% 
    select(-file_path) %>%
    tidyr::unnest("df") %>% 
    mutate(patient_id = paste(site, format_text(MSF_N_Patient), sep = "_")) %>% 
    mutate(db_row = 1:n())
  
  ## columns to add (from original ll template)
  ll_template <- dict_linelist$code_name
  ll_template <- ll_template[!grepl("^MSF_variable_additional", ll_template)]
  cols_to_add <- setdiff(ll_template, names(df_data))
  df_data[cols_to_add] <- NA_character_
  extra_cols <- grep("^extra__", names(df_data), value = TRUE)
  
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
  library(dplyr, warn.conflicts = FALSE)
  library(tidyr, warn.conflicts = FALSE)
  library(glue, warn.conflicts = FALSE)
  library(hmatch, warn.conflicts = FALSE)
  source("R/utilities.R")
  
  # paths to linelist files for given country
  path_data_raw_country <- file.path(path_data_raw, country)
  
  files_country <- list.files(
    path_data_raw_country,
    pattern = glue::glue("^linelist_Covid_anonymous__{country}.*\\.xlsx")
  )
  
  # regex patterns to remove from file path prior to parsing
  reg_rm <- "^linelist_Covid_anonymous__|_[[:digit:]]{2}-[[:digit:]]{2}\\.xlsx"

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
    mutate(site_name_join = format_text2(site_filename)) %>% 
    select(site, country, shape, OC, project, site_name, uid,  site_name_join)

  # parse files and retain only most recent file by site
  df_sheet <- tibble::tibble(file_path = files_country) %>%
    mutate(path_parse = gsub(reg_rm, "", file_path)) %>% 
    mutate(path_parse = gsub("lon_.+_(?=2020)", "0000__", path_parse, perl = TRUE)) %>% 
    tidyr::separate(path_parse, vars_parse, sep = "_{2}") %>% 
    mutate_all(~ ifelse(.x == "", NA_character_, .x)) %>% 
    mutate(site_name_join = format_text2(site_name)) %>% 
    select(-site_name, -project, -key) %>% 
    mutate_all(as.character) %>% 
    left_join(dict_facilities_join, by = c("country", "OC", "site_name_join")) %>% 
    select(-site_name_join) %>% 
    mutate(file_path = file.path(path_data_raw_country, file_path))
  
  if (any(is.na(df_sheet$site))) {
    files_no_match <- unique(basename(df_sheet$file_path[is.na(df_sheet$site)]))
    message("No site match within dict_facilities for file(s):\n- ", paste(files_no_match, collapse = "\n- "))
  }
  
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
read_and_prepare_data <- function(file_path,
                                  site,
                                  dict_extra_vars,
                                  dict_linelist,
                                  cols_derive,
                                  remove_almost_empty = TRUE,
                                  warn_recoded = FALSE) {

  ## requires
  library(dplyr, warn.conflicts = FALSE)
  library(vctrs, warn.conflicts = FALSE)
  library(readxl, warn.conflicts = FALSE)
  library(hmatch, warn.conflicts = FALSE)
  library(janitor, warn.conflicts = FALSE)
  source("R/utilities.R")
  
  ## only for running manually
  if (FALSE) {
    file_path <- df_sheets$file_path[1]
    site <- df_sheets$site[1]
    remove_almost_empty = TRUE
  }
  
  ## read linelist
  df <- readxl::read_xlsx(
    file_path,
    sheet = "linelist",
    col_types = "text",
    na = c("", "NA"),
    .name_repair = ~ vctrs::vec_as_names(..., repair = "unique", quiet = TRUE)
  )
  
  ## remove rows with 3 or fewer non-missing values
  if (remove_almost_empty) {
    df <- df[!almost_empty_rows(df, n_crit = 3), , drop = FALSE]
  }
  
  ## regex for columns to ignore
  regex_exclude <- c(
    "^0\\.\\.",
    "^X0",
    "^Añadir el nombre",
    "^Ajouter le nom",
    "^Add variable name",
    "^MSF_variable_additional",
    "^MSF_test_type_detail",
    "^MSF_lab_date_[2-4]",
    "^MSF_lab_name_[2-4]", 
    "^MSF_sample_type_[2-4]",
    "^MSF_test_type_[2-4]",
    "^MSF_test_results_[2-4]",
    "^MSF_test_other_date",
    "^MSF_test_other_type", 
    "^MSF_test_other_result",
    "^MSF_tb_type",
    "^MSF_outcome_ICU_days", 
    "^MSF_outcome_ventilated_days",
    "^MSF_home_base_care",
    "^MSF_former_status", 
    "^MSF_next status"
  )
  
  ## prepare output
  out_prep <- df %>% 
    select(-matches(paste(regex_exclude, collapse = "|"))) %>%
    mutate_all(as.character) %>% 
    janitor::remove_empty("rows") %>%
    mutate(linelist_row = seq_len(n()), .before = 1)
  
  ## add linelist version and language
  site_meta <- get_site_meta(file_path)
  out_prep$linelist_lang <- rep(site_meta$linelist_lang, nrow(out_prep))
  out_prep$linelist_vers <- rep(site_meta$linelist_vers, nrow(out_prep))
  
  ## rename additional variables
  ll_template <- dict_linelist$code_name
  ll_template <- ll_template[!grepl("^MSF_variable_additional", ll_template)]
  
  out <- out_prep %>% 
    dplyr::rename_with(.fn = recode_columns, dict_extra_vars = dict_extra_vars)
  
  ## check for new additional variable columns to be automatically renamed
  extra_cols <- grep("^extra__", names(out), value = TRUE)
  new_cols <- setdiff(names(out), c(cols_derive, ll_template, extra_cols))
  
  if (length(new_cols) > 0) {
    out <- out %>% 
      dplyr::rename(!!!setNames(new_cols, paste0("extra__", hmatch::string_std(new_cols))))
    
    # print column names automatically recoded as 'extra__'
    if (warn_recoded & length(new_cols) > 0) {
      message("New extra__ linelist columns for site ", site, ": ", vec_paste_c(new_cols))
    }
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
  
  out <- data.frame(orig = x, stringsAsFactors = FALSE) %>% 
    mutate(match = hmatch::string_std(orig)) %>% 
    left_join(dict_extra_vars_prep, by = "match") %>% 
    mutate(orig = ifelse(!is.na(name), name, orig))
  
  return(out$orig)
}

