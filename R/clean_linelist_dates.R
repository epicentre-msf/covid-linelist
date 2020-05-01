#' Clean date columns
#'
#' @param df Linelist data.frame
#' @param path_cleaning_checks Path to directory with cleaning files
#' @param write_checks Logical indicating whether check files should be written
#'
#' @return
#' Linelist with cleaned dates
#' 
clean_linelist_dates <- function(df,
                                 path_cleaning_checks,
                                 write_checks) {
  
  ## requires
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(lubridate)
  library(repi)
  source("R/utilities.R")
  
  ## if running manually
  if (FALSE) {
    df <- df_data_cleaned_numeric
    write_checks <- TRUE
  }
  
  ## gather date columns
  dat_date <- df %>%
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
  df_date_parsed_spread <- dat_date_parsed %>%
    select(-value, -id) %>%
    spread(variable, date)
  
  
  
  
  ## join cleaned data columns to main df
  dat_date_clean <- left_join_replace(df,
                                      df_date_parsed_spread,
                                      cols_match = c("db_row", "patient_id"))
  
  return(dat_date_clean)
}
