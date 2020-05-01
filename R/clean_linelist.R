#' Implement linelist cleaning routines
#'
#' @param df_data 
#' @param dict_factors 
#' @param write_checks 
#'
#' @return
#' Cleaned linelist
#' 
clean_linelist <- function(df_data,
                           path_cleaning,
                           # path_dictionaries,
                           dict_factors,
                           dict_factors_correct,
                           write_checks) {

  ## requires
  library(dplyr)
  library(purrr)
  library(tidyr)
  library(stringr)
  library(readxl)
  library(fs)
  library(forcats)
  library(repi)
  source("R/clean_linelist_dates.R")
  source("R/utilities.R")
  
  
  ## if running manually
  if (FALSE) {
    df_data <- df_data_raw
    write_checks <- TRUE
  }
  
  
  ## remember original column order
  col_order <- names(df_data)
  
  
  ## clean numeric
  dat_numeric <- df_data %>%
    select(db_row,
           patient_id,
           patinfo_ageonset,
           MSF_delay_before_admission,
           MSF_length_stay,
           outcome_contacts_followed) %>% 
    gather(variable, value, -db_row, -patient_id) %>%
    mutate(value = string_to_numeric_prep(value),
           num_value = as.integer(value))
  
  dat_numeric_check <- dat_numeric %>%
    filter(is.na(num_value), !is.na(value))
  
  if (nrow(dat_numeric_check)) {
    message("Problem with numeric values:")
    dat_numeric_check %>%
      knitr::kable(caption = "Problem with numeric values") %>%
      print()
  }

  dat_numeric_merge <- dat_numeric %>%
    select(-value) %>%
    spread(variable, num_value)
  
  
  ## insert cleaned numeric columns back into df_data
  df_data_cleaned_numeric <- left_join_replace(df_data,
                                               dat_numeric_merge,
                                               cols_match = c("db_row", "patient_id"))
  
  
  ## clean dates
  df_dat_cleaned_dates <- clean_linelist_dates(df_data_cleaned_numeric,
                                               # dict_dates,
                                               # path_cleaning_checks,
                                               write_checks = write_checks)
  
  
  ## recode variables using linelist::clean_variable_spelling()
  dict_factors_prep <- dict_factors %>% 
    select(var = variable_en, val = values_en) %>% 
    mutate(val_clean = hmatch::string_std(val)) %>% 
    select(val_clean, val, var)
  
  cols_factor <- unique(dict_factors_prep$var)
  
  df_data_cleaned_factors <- df_dat_cleaned_dates %>% 
    mutate_at(cols_factor, hmatch::string_std) %>% 
    linelist::clean_variable_spelling(wordlists = dict_factors_prep) %>% 
    linelist::clean_variable_spelling(wordlists = dict_factors_correct)
  
  ## arrange columns
  df_dat_cleaned <- df_data_cleaned_factors[,col_order]
  
  
  ## test for valid values
  dict_valid <- dict_factors[,2:1] %>% 
    group_by(variable_en) %>% 
    do(add_na_dict(.)) %>% 
    ungroup()
  
  repi::test_if_valid_multi(df_dat_cleaned, dict_valid)
  
  
  ## return
  return(df_dat_cleaned)
}


