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
    write_checks <- FALSE
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
  }
  
  ## combine
  dat_date_parsed <- bind_rows(dat_date_numeric, dat_date_text)
  
  
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


# keep_ambiguous_dates <- function(df) {
#   
#   library(dplyr)
#   library(stringr)
#   library(lubridate)
#   
#   df %>%
#     filter(
#       (is.na(date) & !is.na(value)) |
#         date < as.Date("2018-05-01") |
#         (date > today() &
#            !(str_detect(patient_id, "katwa|bunia|kayna") & str_detect(variable, "date_lab_sample") & date < (today() + 3)) &
#            !(variable == "date_admission_rep" & date == today() + 1) &
#            !(variable == "date_exit_rep" & date == today() + 1)
#         )
#     )
# }
# 
# 
# flag_ambiguous_dates <- function(df) {
#   
#   library(dplyr)
#   library(stringr)
#   library(lubridate)
#   
#   flag_nonvalid_format <- if_else(!is.na(df$value) & is.na(df$date), TRUE, FALSE)
#   flag_too_early <- if_else(df$date < as.Date("2018-05-01"), TRUE, FALSE)
#   flag_in_future <- if_else((df$date > today() &
#                                !(str_detect(df$patient_id, "katwa|bunia|kayna") & str_detect(df$variable, "date_lab_sample") & df$date < (today() + 3)) &
#                                !(df$variable == "date_admission_rep" & df$date == today() + 1) &
#                                !(df$variable == "date_exit_rep" & df$date == today() + 1)),
#                             TRUE, FALSE)
#   
#   bind_cols(df, tibble(flag_nonvalid_format, flag_too_early, flag_in_future))
# }
# 
# 
# combine_flags <- function(df) {
#   df_flags <- df[,grepl("^flag", names(df))]
#   flags <- gsub("flag_", "", names(df_flags))
#   out <- as.character(apply(df_flags, 1, function(x) paste(flags[which(x)], collapse = ", ")))
#   out[out == ""] <- NA_character_
#   out
# }
# 
# 
# check_files_to_dict <- function(path) {
#   
#   library(readxl)
#   library(dplyr)
#   source("R/utilities.R")
#   
#   file <- tail(strsplit(path, "/")[[1]], 1)
#   
#   read_xlsx(path) %>% 
#     filter(!is.na(check_date), !is.na(date_corrected)) %>% 
#     mutate(date = as.character(date_corrected),   # overwrite date with date_corrected
#            date = ifelse(date == ".na", NA_character_, date),
#            date = vapply(date, parse_excel_dates, ""),
#            file = file) %>% 
#     dplyr::select(patient_id, variable, value, date, file)
# }
# 
# 
# # function to fix issue on date format when using both individual linelist and combined file
# dmy2num <- function(x) {
#   as.numeric(as.Date(dmy(x)) - as.Date("1899-12-30"))
# }

