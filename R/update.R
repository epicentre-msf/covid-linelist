#' Update compiled linelist (wrapper for import, clean, and geocode routines)
#'
#' @param path_data_raw Path to directory with raw linelist files
#' @param path_cleaning Path to directory with data cleaning files
#' @param path_shapefiles Path to directory with shapefiles
#' @param country Country ISO code
#' @param dict_facilities Dictionary mapping site-ID columns (country, OC,
#'   project) to site codes
#' @param dict_numeric_correct Dictionary of corrections for numeric variables
#' @param dict_factors Dictionary of allowed values for all factor variables
#' @param dict_factors_correct Dictionary of corrections for factor variables
#' @param ll_template Vector of column names in original linelist
#' @param run_cleaning Logical indicating whether to run linelist cleaning
#'   function or use last-written cleaned file (defaults to \code{TRUE})
#' @param write_checks Logical indicating whether check files should be written
#'   (defaults to \code{TRUE})
#'
#' @return
#' Compiled, cleaned, geocoded linelist for given country
#' 
update_linelist <- function(path_data_raw,
                            path_cleaning,
                            path_dictionaries,
                            path_shapefiles,
                            country,
                            dict_facilities,
                            dict_factors,
                            dict_extra_vars,
                            ll_template,
                            run_cleaning = TRUE,
                            write_checks = TRUE) {
  
  ## requires
  library(glue)
  library(testthat)
  source("R/import.R")
  source("R/clean.R")
  source("R/geocode.R")
  
  
  ## when running manually
  if (FALSE) {
    run_cleaning <- TRUE
    write_checks <- FALSE
  }
  
  
  ## import and cleaning
  if (run_cleaning) {
    
    # import linelists for each site, with some initial cleaning/standaridizing
    dat_raw <- import_linelists(path_data_raw = path_data_raw,
                                country = country,
                                ll_template = ll_template,
                                dict_facilities = dict_facilities,
                                dict_extra_vars = dict_extra_vars)
    
    # full linelist cleaning
    dat_clean <- clean_linelist(dat_raw,
                                path_cleaning,
                                path_dictionaries,
                                dict_factors = dict_factors,
                                write_checks = write_checks)
    
    # derive age_in_years
    dat_clean$age_in_years <- age_to_years(value = dat_clean$patinfo_ageonset,
                                           unit = dat_clean$patinfo_ageonsetunit)
    
    
    # tests
    if (nrow(dat_raw) != nrow(dat_clean)) warning("nrow(data) changed during cleaning", call. = FALSE)
    
    # write cleaned linelist to file
    saveRDS(dat_clean, glue::glue("local/ll_covid_cleaned_{country}.rds"))

  } else {
    
    # else skip cleaning and use last-written data
    dat_clean <- readRDS(glue::glue("local/ll_covid_cleaned_{country}.rds"))
  }
  
  
  ## geocoding
  dat_geocoded <- clean_geo(dat = dat_clean,
                            path_cleaning = path_cleaning,
                            path_shapefiles = path_shapefiles,
                            country = country,
                            write_checks = write_checks)
  
  
  ## tests
  if (nrow(dat_clean) != nrow(dat_geocoded)) warning("nrow(data) changed during geocoding", call. = FALSE)
  if (any(is.na(dat_geocoded$site))) warning("there are missing values of `site`", call. = FALSE)
  test_duplicated(dat_geocoded$patient_id, "ID")
  
  
  ## return compiled linelist
  return(dat_geocoded)
}

