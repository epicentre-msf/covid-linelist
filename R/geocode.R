#' Implement linelist geocoding routines
#'
#' @param dat Linelist data.frame
#' @param path_cleaning 
#' @param path_shapefiles Path to directory with shapefiles
#' @param country Country ISO code
#' @param write_checks Logical indicating whether check files should be written
#'   (defaults to \code{TRUE})
#'
#' @return
#' Linelist with cleaned admin columns appended
#'
clean_geo <- function(dat,
                      path_cleaning,
                      path_shapefiles,
                      country,
                      write_checks) {
  
  ## requires
  library(dplyr)
  library(glue)
  library(janitor)
  source("R/geocode.R")
  source("R/utilities.R")
  
  
  ## when running manually
  if (FALSE) {
    dat <- dat_clean
  }
  
  
  ## prep dat for geocoding
  col_order <- names(dat)
  adm_cols <- paste0("adm", 1:4, "_name__res")
  
  dat_geoclean_prep <- dat %>% 
    tidyr::separate(MSF_admin_location_past_week,
                    into = adm_cols,
                    sep = "[[:space:]]+\\|[[:space:]]+",
                    fill = "right",
                    remove = FALSE) %>% 
    select(all_of(col_order), matches("^adm[[:digit:]]_name"))
  
  
  ## geo reference file
  georef_file <- file.path(path_shapefiles,
                           country,
                           glue("adm_reference_{country}.rds"))
  
  ## if we have a geo reference DB for given country...
  if (file.exists(georef_file)) {
    
    df_geo_ref <- readRDS(file.path(path_shapefiles,
                                    country,
                                    glue("adm_reference_{country}.rds")))
    
    ## manual corrections
    file_recode <- list_files(path_cleaning,
                              pattern = glue::glue("geocodes_recode_{country}"),
                              full.names = TRUE,
                              last.sorted = TRUE)
    
    dict_recode <- if (length(file_recode) == 1) {
      readxl::read_xlsx(file_recode)
    } else {
      NULL
    }
    
    df_manual_check_full <- list_files(path_cleaning,
                                       pattern = glue("geocodes_check_{country}"),
                                       full.names = TRUE) %>%
      lapply(read_geo_manual) %>%
      dplyr::bind_rows() %>%
      mutate_all(as.character) %>% 
      unique()
    
    df_geo_manual <- if (nrow(df_manual_check_full) && any(!is.na(df_manual_check_full$pcode))) {
      filter(df_manual_check_full, !is.na(pcode))
    } else {
      NULL
    }
    
    df_geo_raw <- dat_geoclean_prep %>% 
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
    
    if (write_checks & nrow(out_check) > 0) {
      file_out <- glue("geocodes_check_{country}_{time_stamp()}.xlsx")
      write_pretty_xlsx(out_check,
                        file = file.path(path_cleaning, file_out),
                        group_shade = "level_ref", zoom = 145)
    }
    
    message(nrow(out_check), " new ambiguous geocodes found")
    
    pcode_bind <- df_match_best %>% 
      filter(!is.na(pcode)) %>% 
      select(starts_with("adm"), pcode) %>% 
      expand_geocode(., "pcode")
    
    for (i in glue("adm{1:4}_pcode")) {
      if (!i %in% names(pcode_bind)) pcode_bind[[i]] <- NA_character_
    }
    
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
    
    df_out <- dat_geoclean_prep %>% 
      left_join(df_geo_pcode, by = c("adm1_name__res", "adm2_name__res", "adm3_name__res", "adm4_name__res")) %>% 
      setNames(gsub("__res$", "__res_raw", names(.))) %>% 
      setNames(gsub("__res_ref$", "__res", names(.))) %>% 
      setNames(gsub("_pcode$", "_pcode__res", names(.))) %>% 
      arrange(db_row)
    
  } else {
    
    ## else, if no georef file...
    df_out <- dat_geoclean_prep %>% 
      setNames(gsub("__res$", "__res_raw", names(.)))
    
    cols_to_add <- c(glue("adm{1:4}_name__res"), glue("adm{1:4}_pcode__res"))
    for (i in cols_to_add) df_out[[i]] <- NA_character_
  }
  
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
