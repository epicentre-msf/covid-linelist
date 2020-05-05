#' Update compiled linelist
#'
#' @param path_data_raw Path to directory with raw linelist files
#' @param dict_facilities 
#' @param dict_factors 
#' @param ll_template 
#' @param country 
#' @param verbose_import 
#' @param run_cleaning Logical indicating whether to run linelist cleaning
#'   function or use last-written cleaned file (defaults to \code{TRUE})
#' @param write_checks Logical indicating whether check files should be written
#'   (defaults to \code{TRUE})
#'
#' @return
#' Tibble reflecting the up-to-date cleaned linelists
#' 
update_linelist <- function(path_data_raw,
                            path_shapefiles,
                            dict_facilities,
                            dict_factors,
                            dict_factors_correct,
                            ll_template,
                            country,
                            run_cleaning = TRUE,
                            write_checks = TRUE,
                            verbose_import = TRUE) {
  
  ## requires
  library(dplyr)
  library(tidyr)
  library(purrr)
  library(glue)
  source("R/import.R")
  source("R/clean_linelist.R")
  source("R/clean_linelist_geo.R")
  
  
  ## when running manually
  if (FALSE) {
    run_cleaning <- TRUE
    run_duplicates <- TRUE
    write_checks <- FALSE
    verbose_import <- FALSE
  }
  
  
  ## import and cleaning
  if (run_cleaning) {
    
    # import linelists for each site, with some initial cleaning/standaridizing
    df_data_raw <- import_linelists(path_data_raw = path_data_raw,
                                    country = country,
                                    ll_template = ll_template,
                                    dict_facilities = dict_facilities)
    
    # full linelist cleaning
    df_data_clean <- clean_linelist(df_data_raw,
                                    dict_factors = dict_factors,
                                    dict_factors_correct = dict_factors_correct,
                                    path_cleaning,
                                    # path_dictionaries,
                                    write_checks = write_checks)
    
    # derived columns
    df_data_clean_derived <- df_data_clean %>% 
      mutate(age_in_years = map2_dbl(patinfo_ageonset, patinfo_ageonsetunit, age_to_years))
    
    # write cleaned linelist to file
    saveRDS(df_data_clean_derived, glue::glue("local/df_data_cleaned_{country}.rds"))

  } else {
    
    # else skip cleaning and use last-written data
    df_data_clean_derived <- readRDS(glue::glue("local/df_data_cleaned_{country}.rds"))
  }
  
  
  ## geo-cleaning
  col_order <- names(df_data_clean_derived)
  adm_cols <- paste0("adm", 1:4, "_name__res")
  
  df_data_geoclean_prep <- df_data_clean_derived %>% 
    tidyr::separate(MSF_admin_location_past_week,
                    into = adm_cols,
                    sep = "[[:space:]]+\\|[[:space:]]+",
                    fill = "right",
                    remove = FALSE) %>% 
    select(all_of(col_order), matches("^adm[[:digit:]]_name"))
  
  df_data_geocleaned <- clean_geo(df_data = df_data_geoclean_prep,
                                  path_cleaning = path_cleaning,
                                  path_shapefiles = path_shapefiles,
                                  country = country)
  
  
  ## return compiled linelist
  return(df_data_geocleaned)
}

