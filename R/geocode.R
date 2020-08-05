#' Implement linelist geocoding routines
#'
#' @param country Country ISO code
#' @param path_shapefiles Path to directory with shapefiles
#' @param path_corrections_geocodes Path to directory with geocode corrections
#' @param write_checks Logical indicating whether check files should be written
#'   (defaults to \code{TRUE})
#'
#' @return
#' Linelist with cleaned admin columns appended
#'
clean_geo <- function(country,
                      path_shapefiles,
                      path_corrections_geocodes,
                      write_checks) {
  
  ## requires
  library(dplyr)
  library(tidyr)
  library(glue)
  library(llutils)
  library(janitor)
  library(hmatch)
  source("R/geocode.R")
  source("R/utilities.R")
  
  ## when running manually
  if (FALSE) {
    country <- "LBN"
    write_checks <- TRUE
  }
  
  ## read cleaned data
  dat <- readRDS(file.path("local", "clean", glue::glue("ll_covid_cleaned_{country}.rds")))
  
  
  
  ## shape
  shape <- unique(dat$shape)
  
  ## prep dat for geocoding
  col_order <- names(dat)
  adm_cols <- paste0("adm", 1:4, "_name__res")
  
  dat_geoclean_prep <- dat %>% 
    tidyr::separate(MSF_admin_location_past_week,
                    into = adm_cols,
                    sep = "[[:space:]]*\\|[[:space:]]*",
                    fill = "right",
                    remove = FALSE) %>% 
    select(all_of(col_order), matches("^adm[[:digit:]]_name")) %>% 
    mutate(across(all_of(adm_cols), ~ ifelse(.x == "", NA_character_, .x)))
  
  
  ## geo reference file
  georef_file <- file.path(
    path_shapefiles,
    shape,
    glue::glue("adm_reference_{shape}.rds")
  )
  
  ## if we have a geo reference DB for given country...
  if (file.exists(georef_file)) {
    
    df_geo_ref <- readRDS(georef_file)
    
    ## manual corrections
    file_recode <- llutils::list_files(
      path_corrections_geocodes,
      pattern = paste0("geocodes_recode", shape),
      full.names = TRUE,
      select = "latest"
    )
    
    dict_recode <- if (length(file_recode) == 1) {
      readxl::read_xlsx(file_recode)
    } else {
      NULL
    }
    
    df_manual_check_full <- llutils::list_files(
      path_corrections_geocodes,
      pattern = paste0("geocodes_check_", shape),
      full.names = TRUE
    ) %>%
      purrr::map_dfr(readxl::read_xlsx, guess_max = 5000) %>%
      mutate(across(where(is.logical), as.character))
    
    if (nrow(df_manual_check_full) == 0) {
      df_manual_check_full <- tibble(
        adm1 = character(0),
        adm2 = character(0),
        adm3 = character(0),
        adm4 = character(0),
        pcode_new = character(0)
      )
    }
    
    df_geo_manual <- df_manual_check_full %>% 
      select(starts_with("adm"), pcode = pcode_new) %>% 
      filter(!is.na(pcode))
    
    df_geo_raw <- dat_geoclean_prep %>% 
      select(matches("^adm[1234]_name")) %>% 
      unique() %>%
      setNames(gsub("_name__res", "", names(.)))
    
    raw_names <- names(df_geo_raw)
    
    df_match_best <- hmatch::hmatch_composite(
      raw = df_geo_raw,
      ref = df_geo_ref,
      man = df_geo_manual,
      pattern = "^adm",
      dict = dict_recode,
      fuzzy = TRUE,
      code_col = "pcode"
    ) %>% 
      select(-level) %>% 
      mutate_all(as.character)
    
    
    if (nrow(df_match_best) > nrow(df_geo_raw)) {
      warning("rows duplicated by hmatch::hmatch(), country ", country, call. = FALSE)
    }
    # update hmatch to handle when adm4 NA and level == 4
    
    ## write file for manual correction
    out_check <- df_match_best %>% 
      janitor::remove_empty("rows") %>% 
      anti_join(df_manual_check_full, by = raw_names) %>%
      filter(is.na(match_type) | grepl("settle", match_type)) %>% 
      mutate(level_raw = best_geolevel(., "^adm[[:digit:]]"),
             level_ref = best_geolevel(., "^ref_adm[[:digit:]]")) %>% 
      arrange(level_ref, adm1, adm2, adm3, adm4) %>% 
      mutate(pcode_new = NA_character_, comment = NA_character_)
    
    if (write_checks & nrow(out_check) > 0) {
      
      # archive previous file
      if (nrow(df_manual_check_full) > 0) {
        file_out_archive <- glue::glue("geocodes_check_{shape}_{time_stamp()}.xlsx")
        
        llutils::write_simple_xlsx(
          df_manual_check_full,
          file = file.path(path_corrections_geocodes, "archive", file_out_archive),
          group = level_raw
        )
      }
        
      # append new checks to old file and write
      dict_geo_correct_write <- df_manual_check_full %>% 
        select(-any_of("i")) %>% 
        mutate(new = NA_character_)
      
      dict_geo_out <- out_check %>% 
        mutate(new = "Yes") %>%
        bind_rows(dict_geo_correct_write, .) %>%
        distinct(adm1, adm2, adm3, adm4, .keep_all = TRUE) %>% 
        arrange(level_raw, level_ref)
      
      file_out <- glue::glue("geocodes_check_{shape}.xlsx")
      
      llutils::write_simple_xlsx(
        dict_geo_out,
        file = file.path(path_corrections_geocodes, file_out),
        group = level_raw
      )
      
      message(nrow(out_check), " new ambiguous geocode(s) written to ", file_out)
    }
    
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
