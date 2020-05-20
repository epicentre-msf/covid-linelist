#' Implement linelist cleaning routines
#'
#' @param dat Linelist data.frame
#' @param path_cleaning Path to directory with data cleaning files
#' @param path_dictionaries Path to directory with recoding dictionaries
#' @param dict_factors Dictionary of allowed values for all factor variables
#' @param write_checks Logical indicating whether check files should be written
#'   (defaults to \code{TRUE})
#'
#' @return
#' Cleaned linelist
#' 
clean_linelist <- function(dat,
                           path_cleaning,
                           path_dictionaries,
                           dict_factors,
                           write_checks) {

  ## requires
  library(dplyr)
  library(tidyr)
  library(glue)
  library(matchmaker)
  source("R/utilities.R")

  ## if running manually
  if (FALSE) {
    dat <- dat_raw
    write_checks <- FALSE
  }
  
  ## linelist language
  lang <- switch(country,
                 "CMR" = "fr",
                 "COD" = "fr",
                 "GIN" = "fr",
                 "MLI" = "fr",
                 "MEX" = "es",
                 "en")
  
  ## read recoding dictionaries
  dict_numeric_correct <- read_xlsx(file.path(path_dictionaries, "dict_numeric_correct.xlsx"))
  dict_factors_correct <- read_xlsx(file.path(path_dictionaries, glue("dict_factors_correct_{lang}.xlsx")))
  
  
  #### Clean numeric variables -------------------------------------------------
  
  ## prepare dictionary
  dict_numeric_prep <- dict_numeric_correct %>% 
    mutate_at(vars(patient_id, variable, value), as.character) %>% 
    mutate_at(vars(replacement), as.integer) %>% 
    filter(!is.na(replacement))
  
  ## gather numeric variables, convert to numeric, and apply dictionary
  dat_numeric <- dat %>%
    select(db_row,
           patient_id,
           patinfo_ageonset,
           MSF_delay_before_admission,
           MSF_length_stay,
           outcome_contacts_followed) %>% 
    tidyr::gather(variable, value, -db_row, -patient_id) %>%
    mutate(value_numeric = suppressWarnings(as.integer(value))) %>% 
    left_join(dict_numeric_prep, by = c("patient_id", "variable", "value")) %>% 
    mutate(replace = !is.na(replacement) & is.na(value_numeric)) %>% 
    mutate(value_numeric = ifelse(replace, replacement, value_numeric)) %>% 
    select(-replacement) %>% 
    mutate(flag = !is.na(value) & is.na(value_numeric))
  
  ## check for non-missing values not converted to numeric
  if (any(dat_numeric$flag)) {
    message("Values of numeric variables could not be coerced to numeric:")
    dat_numeric %>%
      filter(flag) %>% 
      select(patient_id, variable, value) %>% 
      mutate(replace = NA_character_) %>% 
      print()
  }
  
  ## merge cleaned numeric columns back into dat_numeric
  dat_numeric_clean <- dat_numeric %>%
    select(-value, -replace, -flag) %>%
    tidyr::spread(variable, value_numeric) %>% 
    left_join_replace(x = dat, y = ., cols_match = c("db_row", "patient_id"))
  
  
  #### Clean date variables ----------------------------------------------------
  
  ## gather date columns
  dat_date <- dat_numeric_clean %>%
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
    tidyr::gather(variable, value, -db_row, -patient_id) %>%
    arrange(db_row, patient_id) %>% 
    mutate(date = as.Date(value)) %>%
    mutate(flag = !is.na(value) & is.na(date))
  
  
  ## check for non-missing values not converted to date
  if (any(dat_date$flag)) {
    
    dat_date_flag <- dat_date %>% 
      filter(flag) %>% 
      select(db_row) %>%
      unique() %>% 
      left_join(dat_date, by = "db_row") %>% 
      mutate(flag = ifelse(!flag, NA, flag)) %>% 
      filter(!is.na(value)) %>% 
      group_by(db_row) %>% 
      arrange(date) %>% 
      ungroup() %>% 
      mutate(date_correct = NA_character_, comment = NA_character_) %>% 
      select(patient_id, variable, value, date, date_correct, flag, comment)
      
    file_out <- glue("dates_check_{country}_{time_stamp()}.xlsx")
    
    write_pretty_xlsx(dat_date_flag,
                      file = file.path(path_cleaning, file_out),
                      group_shade = "patient_id")
  }
  
  ## merge cleaned date columns back into dat
  dat_dates_clean <- dat_date %>%
    select(-value, -flag) %>%
    tidyr::spread(variable, date) %>% 
    left_join_replace(x = dat_numeric_clean, y = ., cols_match = c("db_row", "patient_id"))
  
  ## possible additional date checks to implement
  # all date variables <= today
  # all date variables >= 2020-02-01
  # report_date > patcourse_dateonset
  # outcome_onset_symptom >= patcourse_dateonset
  
  
  #### Clean categorical variables ---------------------------------------------
  
  ## recode variables using matchmaker::match_df()
  dict_factors_prep <- dict_factors[,c("variable",
                                       paste("values", lang, sep = "_"),
                                       "values_en")] %>% 
    setNames(c("var", "val", "val_en")) %>% 
    mutate(val_clean = hmatch::string_std(val)) %>% 
    select(val_clean, val, var, val_en)
  
  dat_factors_clean <- dat_dates_clean %>% 
    mutate_at(unique(dict_factors$variable), hmatch::string_std) %>% 
    matchmaker::match_df(dictionary = dict_factors_prep) %>% 
    matchmaker::match_df(dictionary = dict_factors_correct)
  
  ## test for nonvalid values
  nonvalid <- matchmaker::check_df(dat_factors_clean,
                                   dict_factors_prep,
                                   col_vals = 2,
                                   col_vars = 3,
                                   always_allow_na = TRUE,
                                   return_allowed = TRUE)
  
  if (nrow(nonvalid) > 0) {
    message("Some categorical variables contain nonvalid values:")
    print(nonvalid)
  }
  
  ## translate to english
  dat_factors <- dat_factors_clean %>% 
    matchmaker::match_df(dictionary = dict_factors_prep, from = "val", to = "val_en")
  
  ## return
  return(dat_factors)
}
