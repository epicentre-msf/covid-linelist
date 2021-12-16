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
    country <- "CAF"
    site_exclude <- NULL
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
    ## account for export from SYR_A_RNH which was split to limit file size
    filter(!(site == "SYR_A_RNH" & lubridate::as_date(upload_date) < as.Date("2021-03-10") & as_integer_quiet(gsub("R0000", "", MSF_N_Patient)) >= 2870))
  
  ## check for valid values of MSF_name_facility at site SYR_P_ALA
  if ("SYR_P_ALA" %in% df_data$site) {
    
    # file to extract MSF_N_Patient - MSF_name_facility combinations
    file_map_SYR_P_ALA <- file.path(
      path_data_raw,
      "SYR",
      "linelist_Covid_anonymous__SYR__OCP____Covid Treatment Center__Alameen CCTC__1676__2021-03-17_13-15.xlsx"
    )
    
    # extract MSF_N_Patient - MSF_name_facility combinations
    map_SYR_P_ALA <- readxl::read_xlsx(
      file_map_SYR_P_ALA,
      sheet = "linelist",
      col_types = "text",
      na = c("", "NA"),
      .name_repair = ~ vctrs::vec_as_names(..., repair = "unique", quiet = TRUE)
    ) %>% 
      distinct(MSF_N_Patient, MSF_name_facility)
    
    # recode MSF_name_facility for site 
    df_data <- df_data %>% 
      llutils::recode_conditional(map_SYR_P_ALA, col_recode = "MSF_name_facility")
    
    # check for non-valid sites
    non_valid_site <- df_data %>% 
      filter(site == "SYR_P_ALA", !MSF_name_facility %in% c("Al-Ameen OPD", "Al-Ameen CCTC", "Al-Ameen MC")) %>% 
      count(MSF_name_facility)
    
    if (nrow(non_valid_site) > 0) {
      warning(
        "Non-standard values of MSF_name_facility for site SYR_P_ALA:\n",
        print_and_capture(non_valid_site),
        call. = FALSE
      )
    }
    
    df_data <- df_data %>% 
      filter(!(site %in% "SYR_P_ALA" & is.na(MSF_name_facility)))
  }
  
  df_data <- df_data %>% 
    mutate(
      ## account for SYR_P_ALA which contains 3 sites
      site_name = case_when(
        site == "SYR_P_ALA" ~ MSF_name_facility,
        TRUE ~ site_name
      ),
      ## avoid duplicate patients IDs with prev non-intersect. linelist for YEM_P_SAN
      MSF_N_Patient = case_when(
        site %in% "YEM_P_SAN" ~ paste0("N00", MSF_N_Patient),
        TRUE ~ MSF_N_Patient
      ),
      ## avoid duplicate patients IDs with prev intersect. linelist for CAF_E_BAT
      MSF_N_Patient = case_when(
        site %in% "CAF_E_BAT" & lubridate::as_date(upload_date) > as.Date("2021-12-01") ~ paste0("LL2_", MSF_N_Patient),
        TRUE ~ MSF_N_Patient
      )
    ) %>% 
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
  library(rlang, warn.conflicts = FALSE)
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
  reg_rm <- "^linelist_Covid_anonymous__|_\\d{2}-\\d{2}\\.xlsx$|\\.xlsx$"
  
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
    filter(country == .env$country) %>%
    mutate_all(as.character) %>% 
    mutate(split1 = lubridate::as_date(split1)) %>% 
    mutate(site_name_join = format_text2(site_filename)) %>% 
    select(site, source, country, shape, OC, project, site_name, uid,  site_name_join, split1)
  
  # parse files and retain only most recent file by site
  df_sheet <- tibble::tibble(file_path = files_country) %>%
    mutate(path_parse = gsub(reg_rm, "", file_path)) %>% 
    mutate(path_parse = gsub("lon_.+_(?=202[0123])", "0000__", path_parse, perl = TRUE)) %>% 
    tidyr::separate(path_parse, vars_parse, sep = "_{2}") %>% 
    mutate_all(~ ifelse(.x == "", NA_character_, .x)) %>% 
    mutate(site_name_join = format_text2(site_name)) %>% 
    select(-site_name, -project, -key) %>% 
    mutate_all(as.character) %>% 
    left_join(dict_facilities_join, by = c("country", "OC", "site_name_join")) %>% 
    select(-site_name_join) %>% 
    mutate(file_path = file.path(path_data_raw_country, file_path)) %>% 
    mutate(group = as.integer(lubridate::as_date(upload_date) > split1))
  
  if (any(is.na(df_sheet$site))) {
    files_no_match <- unique(basename(df_sheet$file_path[is.na(df_sheet$site)]))
    message("No site match within dict_facilities for file(s):\n- ", paste(files_no_match, collapse = "\n- "))
  }
  
  if (return_latest) {
    df_sheet <- df_sheet %>% 
      group_by(site, source, group) %>%
      arrange(desc(upload_date)) %>% 
      slice(1) %>%
      ungroup() %>% 
      select(-any_of(c("split1", "group", "source")))
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
    file_path <- df_sheets$file_path[6]
    site <- df_sheets$site[6]
    remove_almost_empty = TRUE
  }
  
  ## read linelist
  df <- readxl::read_xlsx(
    file_path,
    sheet = "linelist",
    col_types = "text",
    na = c("", "NA"),
    .name_repair = ~ vctrs::vec_as_names(..., repair = "unique", quiet = TRUE)
  ) %>% 
    select(-starts_with("\u0625"))
  
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
    "^MSF_variable_additional"
  )
  
  ## prepare output
  out_prep <- df %>% 
    select(-matches(paste(regex_exclude, collapse = "|"))) %>%
    mutate_all(as.character) %>% 
    janitor::remove_empty("rows") %>%
    mutate(linelist_row = seq_len(n()), .before = 1)
  
  # filter MWI Health Center sites based on MSF_name_facility
  if (site %in% "MWI_P_CHI") out_prep <- filter(out_prep, MSF_name_facility %in% "Chilomoni Health Centre | Chilomoni Ward")
  if (site %in% "MWI_P_SLZ") out_prep <- filter(out_prep, MSF_name_facility %in% "South Lunzu Health Centre | South Lunzu Ward")
  
  
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


