#' Implement linelist cleaning routines
#'
#' @param dat Linelist data.frame
#' @param path_cleaning 
#' @param dict_numeric_correct Dictionary of corrections for numeric variables
#' @param dict_factors Dictionary of allowed values for all factor variables
#' @param dict_factors_correct Dictionary of corrections for factor variables
#' @param write_checks 
#'
#' @return
#' Cleaned linelist
#' 
clean_linelist <- function(dat,
                           path_cleaning,
                           dict_numeric_correct,
                           dict_factors,
                           dict_factors_correct,
                           write_checks) {

  ## requires
  library(dplyr)
  library(purrr)
  library(tidyr)
  library(stringr)
  library(lubridate)
  library(readxl)
  library(fs)
  library(forcats)
  library(repi)
  source("R/clean.R")
  source("R/utilities.R")

  
  ## if running manually
  if (FALSE) {
    dat <- df_data_raw
    write_checks <- FALSE
  }
  
  
  ## remember original column order
  col_order <- names(dat)
  
  
  #### Clean numeric variables -------------------------------------------------
  dat_numeric <- dat %>%
    select(db_row,
           patient_id,
           patinfo_ageonset,
           MSF_delay_before_admission,
           MSF_length_stay,
           outcome_contacts_followed) %>% 
    gather(variable, value, -db_row, -patient_id) %>%
    mutate(value = string_to_numeric_prep(value),
           num_value = as.integer(value))
  
  ## check for non-missing values that could not be converted to numeric
  dat_numeric_check <- dat_numeric %>%
    filter(is.na(num_value), !is.na(value))
  
  if (nrow(dat_numeric_check)) {
    message("Some values could not be coerced to numeric:")
    dat_numeric_check %>%
      select(-db_row, -num_value) %>% 
      mutate(replace = NA_character_) %>% 
      as.data.frame() %>% 
      print()
  }
  
  ## insert cleaned numeric columns back into df_data
  dat_numeric_merge <- dat_numeric %>%
    select(-value) %>%
    spread(variable, num_value)
  
  dat_cleaned_numeric <- left_join_replace(
    dat,
    dat_numeric_merge,
    cols_match = c("db_row", "patient_id")
  )
  
  #### Clean date variables ----------------------------------------------------
  
  ## gather date columns
  dat_date <- dat_cleaned_numeric %>%
    select(db_row,
           patient_id,
           report_date,
           Lab_date1,
           patcourse_dateonset,
           ends_with("date_onset"),
           MSF_date_consultation,
           patcourse_presHCF,
           patcourse_dateiso,
           starts_with("expo_travel_date"),
           starts_with("expo_case_date"),
           outcome_submitted_date,
           outcome_patcourse_presHCF,
           outcome_date_of_outcome,
           outcome_lab_date) %>%
    gather(variable, value, -db_row, -patient_id) %>%
    mutate(id = 1:n()) %>% 
    arrange(db_row, patient_id, id)
  
  
  ## parse numeric values to date (anything that can't be parsed converted to <NA>)
  dat_date_numeric <- dat_date %>%
    mutate(date = as.Date(value)) %>%
    filter(is.na(value) | !is.na(date))
  
  
  ## try parsing values converted to <NA> above
  dat_date_text <- dat_date %>%
    anti_join(dat_date_numeric, "id") %>%
    mutate(date = parse_date_time(value, orders = c("dby", "bdy"), quiet = TRUE),
           date = as.Date(date),
           date = if_else(is.na(date) & str_detect(value, "\\d+/\\d+/\\d+"), value %>% mdy(quiet = TRUE) %>% as.Date(), date)
    )
  
  
  if (nrow(dat_date_text) > 0) {
    message("Check date values that could not initially be parsed")
    print(dat_date_text, n = "all")
  }
  
  
  ## combine
  dat_date_parsed <- bind_rows(dat_date_numeric, dat_date_text)
  
  
  # ## recode based on all dictionaries
  # dat_parsed_recoded <- dat_date_parsed %>%  
  #   repi::recode_conditional(dict = dict_dates, col_recode = "date", flag_recoded = TRUE)
  
  
  ## spread date columns
  dat_date_parsed_spread <- dat_date_parsed %>%
    select(-value, -id) %>%
    spread(variable, date)
  
  
  ## join cleaned data columns to main df
  dat_cleaned_dates <- left_join_replace(
    dat_cleaned_numeric,
    dat_date_parsed_spread,
    cols_match = c("db_row", "patient_id")
  )
  
  
  #### Clean factor variables --------------------------------------------------
  
  ## recode variables using linelist::clean_variable_spelling()
  dict_factors_prep <- dict_factors %>% 
    select(var = variable_en, val = values_en) %>% 
    mutate(val_clean = hmatch::string_std(val)) %>% 
    select(val_clean, val, var)
  
  cols_factor <- unique(dict_factors_prep$var)
  
  dat_cleaned_factors <- dat_cleaned_dates %>% 
    mutate_at(cols_factor, hmatch::string_std) %>% 
    linelist::clean_variable_spelling(wordlists = dict_factors_prep) %>% 
    linelist::clean_variable_spelling(wordlists = dict_factors_correct)
  
  
  ## arrange columns
  dat_cleaned <- dat_cleaned_factors[,col_order]
  
  
  ## test for valid values
  dict_valid <- dict_factors[,2:1] %>% 
    group_by(variable_en) %>% 
    do(add_na_dict(.)) %>% 
    ungroup()
  
  repi::test_if_valid_multi(dat_cleaned, dict_valid)
  
  
  ## return
  return(dat_cleaned)
}
