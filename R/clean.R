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
  library(repi)
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
                 "NER" = "fr",
                 "HTI" = "fr",
                 "CAF" = "fr",
                 "MEX" = "es",
                 "VEN" = "es",
                 "en")
  
  ## read recoding dictionaries
  dict_numeric_correct <- read_xlsx(file.path(path_dictionaries, "dict_numeric_correct.xlsx"))
  dict_factors_correct <- read_xlsx(file.path(path_dictionaries, glue("dict_factors_correct_{lang}.xlsx")))
  
  
  #### Clean numeric variables -------------------------------------------------
  
  ## prepare dictionary
  dict_numeric_prep <- dict_numeric_correct %>% 
    mutate_at(vars(patient_id, variable, value), as.character) %>% 
    mutate_at(vars(replacement), as.integer) %>% 
    mutate(replace = TRUE)
    
  
  ## gather numeric variables, convert to numeric, and apply dictionary
  dat_numeric <- dat %>%
    select(db_row,
           patient_id,
           patinfo_ageonset,
           MSF_delay_before_admission,
           MSF_length_stay,
           Comcond_present,
           outcome_contacts_followed) %>% 
    tidyr::gather(variable, value, -db_row, -patient_id) %>%
    mutate(value_numeric = suppressWarnings(as.integer(value))) %>% 
    left_join(dict_numeric_prep, by = c("patient_id", "variable", "value")) %>% 
    mutate(replace = ifelse(is.na(replace), FALSE, replace)) %>% 
    mutate(value_numeric = ifelse(replace, replacement, value_numeric)) %>% 
    select(-replacement) %>% 
    mutate(flag = !is.na(value) & is.na(value_numeric) & !replace)
  
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
  
  ## assemble dictionary for date variables
  check_files <- list_files(
    path_cleaning,
    pattern = glue::glue("dates_check_{country}_.*\\.xlsx"),
    full.names = TRUE
  )
  
  dict_dates <- dplyr::bind_rows(lapply(check_files, check_files_to_dict))
  if (nrow(dict_dates) == 0) dict_dates <- NULL
  
  
  ## gather date columns
  dat_date <- dat_numeric_clean %>%
    select(db_row,
           patient_id,
           upload_date,
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
           outcome_lab_date,
           MSF_date_treament1,
           MSF_date_treament2,
           MSF_date_treament3) %>%
    tidyr::gather(variable, value, -db_row, -patient_id) %>%
    arrange(db_row, patient_id) %>% 
    mutate(date = as.Date(value))
  
  if (!is.null(dict_dates)) {
    dat_date <- dat_date %>% 
      repi::recode_conditional(dict = dict_dates, col_recode = "date", flag_recoded = TRUE)
  } else {
    dat_date <- dat_date %>% 
      mutate(date_is_recoded = FALSE)
  }
  
  dat_date <- dat_date %>% 
    mutate(flag_ambiguous = ifelse(!date_is_recoded & !is.na(value) & is.na(date), "flag_ambiguous", NA)) %>% 
    select(-date_is_recoded)
  

  #### Other date-logic checks -------------------------------------------------
  
  # define flag-variable mappings
  df_flags <- tibble(
    flag = c(rep("flag_consult_before_onset", 2),
             rep("flag_report_before_consult", 2),
             rep("flag_outcome_before_consult", 2),
             rep("flag_submit_before_consult", 2),
             rep("flag_upload_before_report", 2)),
    variable = c("MSF_date_consultation", "patcourse_dateonset",     # check if flag_consult_before_onset
                 "report_date", "MSF_date_consultation",             # check if flag_report_before_consult
                 "outcome_date_of_outcome", "MSF_date_consultation", # check if flag_outcome_before_consult
                 "outcome_submitted_date", "MSF_date_consultation",
                 "upload_date", "report_date"),                      # check if flag_upload_before_report
    value = TRUE, check_date = TRUE
  )
  
  dat_date_flags <- dat_date %>%
    select(-value, -flag_ambiguous) %>%
    tidyr::spread(variable, date) %>% 
    mutate(flag_consult_before_onset = MSF_date_consultation < patcourse_dateonset,
           flag_report_before_consult = report_date < MSF_date_consultation,
           flag_outcome_before_consult = outcome_date_of_outcome < MSF_date_consultation,
           flag_submit_before_consult = outcome_submitted_date < MSF_date_consultation,
           flag_upload_before_report = upload_date < report_date) %>% 
    select(db_row, patient_id, starts_with("flag")) %>% 
    tidyr::gather(flag, value, -db_row, -patient_id) %>% 
    mutate(value = ifelse(!value, NA, value)) %>% 
    left_join(df_flags, by = c("flag", "value")) %>% 
    filter(value) %>% 
    group_by(db_row, patient_id, variable) %>% 
    summarize(flag = paste(unique(flag), collapse = "; "), .groups = "keep") %>% 
    ungroup()

  dates_check <- dat_date %>% 
    mutate(flag_future = ifelse(date > lubridate::today(), "flag_future", NA_character_),
           flag_too_early = ifelse(date < as.Date("2020-02-01"), "flag_too_early", NA_character_)) %>% 
    left_join(dat_date_flags, by = c("db_row", "patient_id", "variable")) %>% 
    tidyr::unite("flag", flag_ambiguous, flag_too_early, flag_future, flag, na.rm = TRUE, sep = "; ") %>% 
    mutate(flag = ifelse(flag == "", NA_character_, gsub("flag_", "", flag))) %>% 
    group_by(db_row, patient_id) %>% 
    mutate(check = any(!is.na(flag))) %>% 
    ungroup() %>% 
    filter(check) %>% 
    select(-check) %>% 
    filter(!is.na(value) | !is.na(date)) %>% 
    arrange(db_row, date) %>% 
    mutate(date_correct = NA_character_, comment = NA_character_) %>% 
    select(patient_id, variable, value, date, date_correct, flag, comment)
  

  ## check for non-missing values not converted to date
  if (nrow(dates_check) > 0) {
    
    write_pretty_xlsx(
      dates_check,
      file = file.path(path_cleaning, glue("dates_check_{country}_{time_stamp()}.xlsx")),
      group_shade = "patient_id"
    )
    
    nambig <- sum(!is.na(dates_check$flag), na.rm = TRUE)
    message(paste(nambig, "ambiguous dates written to file"))
  }
  
  
  ## merge cleaned date columns back into dat
  dat_dates_clean <- dat_date %>%
    select(-value, -flag_ambiguous) %>%
    tidyr::spread(variable, date) %>% 
    left_join_replace(x = dat_numeric_clean, y = ., cols_match = c("db_row", "patient_id"))
  
  
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
