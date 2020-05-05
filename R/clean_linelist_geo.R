#' Wrapper for geocleaning/geocoding routines
#'
#' @param df_data 
#' @param path_cleaning 
#'
#' @return
#' @export
#'
#' @examples
clean_geo <- function(df_data,
                      path_cleaning,
                      path_shapefiles,
                      country) {
  
  ## requires
  library(dplyr)
  library(glue)
  library(janitor)
  source("R/clean_linelist_geo.R")
  source("R/utilities.R")
  
  
  ## when running manually
  if (FALSE) {
    df_data <- df_data_geoclean_prep
  }
  
  
  ## load reference DB
  df_geo_ref <- file.path(path_shapefiles, country, glue("adm_reference_{country}.rds")) %>% 
    readRDS()
  
  ## manual corrections
  file_recode <- file.path(path_cleaning, country) %>% 
    list_files(pattern = glue::glue("geocodes_recode_{country}"), full.names = TRUE, last.sorted = TRUE)
  
  dict_recode <- if (length(file_recode) == 1) {
    readxl::read_xlsx(file_recode)
  } else {
    NULL
  }
  
  df_manual_check_full <- file.path(path_cleaning, country) %>% 
    list_files(., pattern = glue::glue("geocodes_check_{country}"), full.names = TRUE) %>%
    lapply(read_geo_manual) %>%
    dplyr::bind_rows() %>%
    mutate_all(as.character) %>% 
    unique()

  df_geo_manual <- if (nrow(df_manual_check_full) && any(!is.na(df_manual_check_full$pcode))) {
    filter(df_manual_check_full, !is.na(pcode))
  } else {
    NULL
  }
  
  df_geo_raw <- df_data %>% 
    select(matches("^adm[1234]_name")) %>% 
    unique() %>%
    setNames(gsub("_name__res", "", names(.)))

  raw_names <- names(df_geo_raw)
  
  df_match_best <- hmatch::hmatch(raw = df_geo_raw,
                                  ref = df_geo_ref,
                                  man = df_geo_manual,
                                  pattern_raw = "^adm",
                                  dict = dict_recode,
                                  fuzzy = TRUE,
                                  code_col = "pcode") %>% 
    select(-level)
  
  ## write file for manual correction
  df_manual_check_full_join <- df_manual_check_full %>%
    select(starts_with("adm"))
  
  if (nrow(df_manual_check_full_join) == 0) {
    df_manual_check_full_join <- tibble(
      adm1 = character(0),
      adm2 = character(0),
      adm3 = character(0),
      adm4 = character(0)
    )
  }
  
  out_check <- df_match_best %>% 
    anti_join(df_manual_check_full_join, by = raw_names) %>%
    filter(is.na(match_type) | match_type %in% c("best_single", "best_multi")) %>% 
    mutate(level_raw = best_geolevel(., "^adm[[:digit:]]"),
           level_ref = best_geolevel(., "^ref_adm[[:digit:]]")) %>% 
    arrange(level_ref, adm1, adm2, adm3, adm4) %>% 
    mutate(pcode_new = NA_character_, comment = NA_character_)
  
  if (nrow(out_check) > 0) {
    write_pretty_xlsx(out_check,
                      file = file.path(file.path(path_cleaning, country), glue::glue("geocodes_check_{country}_{time_stamp()}.xlsx")),
                      group_shade = "level_ref", zoom = 145)
  }
  
  message(nrow(out_check), " new ambiguous geocodes found")
  
  pcode_bind <- df_match_best %>% 
    filter(!is.na(pcode)) %>% 
    select(starts_with("adm"), pcode) %>% 
    expand_geocode(., "pcode")
  
  df_geo_pcode <- df_geo_raw %>% 
    left_join(pcode_bind, by = raw_names) %>% 
    filter(!is.na(pcode)) %>% 
    rename(adm1_name = adm1,
           adm2_name = adm2,
           adm3_name = adm3,
           adm4_name = adm4) %>% 
    setNames(gsub("_name$", "_name__res", names(.))) %>%
    left_join(df_geo_ref, by = "pcode") %>% 
    rename(adm1_name__res_ref = adm1,
           adm2_name__res_ref = adm2,
           adm3_name__res_ref = adm3,
           adm4_name__res_ref = adm4) %>% 
    select(-pcode, -level) %>% 
    select(ends_with("__res"),
           ends_with("ref"),
           ends_with("pcode"))
  
  df_out <- df_data %>% 
    left_join(df_geo_pcode, by = c("adm1_name__res", "adm2_name__res", "adm3_name__res", "adm4_name__res")) %>% 
    setNames(gsub("__res$", "__res_raw", names(.))) %>% 
    setNames(gsub("__res_ref$", "__res", names(.))) %>% 
    setNames(gsub("_pcode$", "_pcode__res", names(.))) %>% 
    arrange(db_row)
  
  return(df_out)
}




## helpers
read_geo_manual <- function(path) {
  readxl::read_xlsx(path) %>% 
    select(starts_with("adm"), pcode = pcode_new)
}


expand_geocode <- function(dat, code_col, split = "__") {
  bind_list <- lapply(dat[[code_col]], expand_geocode_row)
  bind_df <- dplyr::bind_rows(bind_list)
  dplyr::bind_cols(dat, bind_df)
}


expand_geocode_row <- function(code, levels = 4, split = "__") {
  code_split <- strsplit(code, split)[[1]]
  levels <- seq_len(levels)
  x <- vapply(levels, expand_geocode_helper, "", code = code, split = split)
  as.data.frame(t(setNames(x, paste0("adm", levels, "_pcode"))), stringsAsFactors = FALSE)
}


expand_geocode_helper <- function(code, level, split) {
  code_split <- strsplit(code, split)[[1]]
  code_level <- length(code_split)
  if (level > code_level | is.na(code)) {
    return(NA_character_)
  } else {
    return(paste(code_split[1:level], collapse = split))
  }
}


# TODO: remove adm_match from arguments and automatically find them by looking
# at admX_nam+suffix in data check that we don't erase existing variables if
# pcode already present => throw error or warning
add_pcode <- function(df_data, df_geo_pcode, adm_match, suffix = "") {
  
  
  suffix = paste0("__", c("res"))
  
  
  library(dplyr)
  library(rlang)
  
  var_pcode <- df_geo_pcode %>%
    select(-ends_with("name$")) %>%
    names()
  
  one_suffix <- suffix
  for(one_suffix in suffix) {
    
    # rename df_geo_pcode
    names(var_pcode) <- paste0(var_pcode, one_suffix)
    df_geo_pcode_suffix <- df_geo_pcode %>%
      rename(!!var_pcode)
    
    # rename df_data
    var_data <- paste0(adm_match, one_suffix)
    names(var_data) <- paste0(var_data, "_raw")
    df_data <- df_data %>% rename(!!var_data)
    
    by <- paste0(adm_match, "_raw")
    names(by) <- names(var_data)
    
    df_data <- df_data %>%
      left_join(df_geo_pcode_suffix, by = by)
  }
  
  return(df_data)
}
