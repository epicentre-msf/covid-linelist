
#' Prepare MSF Covid linelist data
#'
#' @param dta The raw linelist downloaded
#'
#' @return
#' @export
#'
prepare_msf_dta <- function(dta) {
  
  if (FALSE) {
    dta <- d_global
  }
  
  ## create copies of indicator vars with prefix ind_
  indicator_vars <- dta %>% 
    select(Comcond_preg:MSF_smoking) %>% 
    setNames(paste0("ind_", names(.)))
  
  dta <- bind_cols(dta, indicator_vars)
  
  ## --- CLEAN ---
  dta <- dta %>%
    mutate(
      age_in_years = case_when(
        age_in_years > 110 ~ NA_real_,
        TRUE ~ age_in_years
      ),
      ind_Comcond_immuno = case_when(
        grepl('Positive', MSF_hiv_status) ~ 'Yes',
        TRUE ~ ind_Comcond_immuno
      ),
      ind_Comcond_cardi = case_when(
        MSF_hypertension == 'Yes' ~ 'Yes',
        TRUE ~ ind_Comcond_cardi
      ),
      ind_Comcond_other = case_when(
        tolower(Comcond_other) %in% c("no", "n", "nil", "non", "ras", "ninguna") ~ "No",
        tolower(Comcond_other) %in% "unknown" ~ "Unknown",
        !is.na(Comcond_other) ~ 'Yes',
        TRUE ~ "Unknown"
      )
    )
  
  # dta %>%
  #   mutate(Comcond_other = str_squish(tolower(Comcond_other))) %>% 
  #   count(Comcond_other, ind_Comcond_other, sort = TRUE)
  
  ## --- PREPARE ---
  
  ## Create variable levels
  levels_covid_status <- c('Confirmed', 'Probable', 'Suspected', 'Not a case', 'Not a suspect', '(Unknown)') # PB add 'Not a suspect'
  levels_outcome_status <- c('Cured', 'Died', 'Left against medical advice', 'Transferred', 'Sent back home', 'Other')
  levels_ynu <- c('Yes', 'No', '(Unknown)')
  
  ## Factorise variables
  dta <- dta %>%
    mutate(
      ind_MSF_covid_status = factor(MSF_covid_status, levels = levels_covid_status) %>% forcats::fct_explicit_na(na_level = '(Unknown)'),
      ind_outcome_patcourse_status = factor(outcome_patcourse_status, levels = levels_outcome_status) %>% forcats::fct_explicit_na(na_level = '(Pending/Unknown)'),
      ind_patcourse_admit = factor(patcourse_admit, levels = levels_ynu) %>% forcats::fct_explicit_na(na_level = '(Unknown)'),
      ind_outcome_patcourse_admit = factor(outcome_patcourse_admit, levels = levels_ynu) %>% forcats::fct_explicit_na(na_level = '(Unknown)'),
    )
  
  ## Standardise dates (and weeks)
  dta <- dta %>%
    mutate(
      # MSF_date_consultation = as.Date(MSF_date_consultation),
      epi_week_report = monday_week_date(report_date),
      epi_week_consultation = monday_week_date(MSF_date_consultation),
      epi_week_admission = monday_week_date(patcourse_presHCF),
      epi_week_onset = monday_week_date(patcourse_dateonset)
    )
  
  ## Create age groups
  age_breaks_5 <- c(0, 5, 15, 45, 65, Inf)
  age_labels_5 <- label_breaks(age_breaks_5, exclusive = TRUE)
  
  age_breaks_9 <- c(seq(0, 80, 10), Inf)
  age_labels_9 <- label_breaks(age_breaks_9, exclusive = TRUE)
  
  dta <- dta %>%
    mutate(
      age_5gp = cut(floor(age_in_years), breaks = age_breaks_5, labels = age_labels_5, include.lowest = TRUE, right = FALSE),
      age_9gp = cut(floor(age_in_years), breaks = age_breaks_9, labels = age_labels_9, include.lowest = TRUE, right = FALSE)
    )
  
  ## Recoding Comorbidities as Yes/No
  dta <- dta %>%
    mutate(
      ind_MSF_malaria = case_when(
        ind_MSF_malaria == 'Positive' ~ 'Yes',
        ind_MSF_malaria == 'Negative' ~ 'No',
        ind_MSF_malaria %in% c('Inconclusive', 'Not done') ~ 'Unknown',
        TRUE ~ ind_MSF_malaria
      ),
      ind_MSF_hiv_status = case_when(
        ind_MSF_hiv_status %in% c('Positive (no ARV)', 'Positive (on ART)', 'Positive (unknown)') ~ 'Yes',
        ind_MSF_hiv_status == 'Negative' ~ 'No',
        TRUE ~ ind_MSF_hiv_status
      ),
      ind_MSF_tb_active = case_when(
        ind_MSF_tb_active %in% c('Yes (currently no treatment)', 'Yes (currently on treatment)', 'Yes (unknown)') ~ 'Yes',
        TRUE ~ ind_MSF_tb_active
      ),
      ind_MSF_smoking = case_when(
        ind_MSF_smoking %in% c('Yes (current)', 'Yes (former)') ~ 'Yes',
        TRUE ~ ind_MSF_smoking
      )
    )
  
  
  ## Recode presence of comorbidities including the MSF ones
  comcond_count_prep <- dta %>% 
    select(
      starts_with('ind_Comcond_'),
      ind_MSF_hiv_status,
      ind_MSF_hypertension,
      ind_MSF_tb_active,
      ind_MSF_malaria,
      ind_MSF_malnutrition,
      ind_MSF_smoking
    ) %>% 
    select(-ind_Comcond_pregt)
  
  ## Patients' care variables
  dta <- dta %>%
    mutate(
      ind_Comcond_count = rowSums(comcond_count_prep == "Yes", na.rm = TRUE),
      ind_Comcond_01 = ifelse(ind_Comcond_count > 0, 1, 0)
    ) %>% 
    mutate(
      merge_admit = case_when(
        ind_patcourse_admit == 'Yes' | ind_outcome_patcourse_admit == 'Yes' ~ levels_ynu[1],
        ind_patcourse_admit == '(Unknown)' & ind_outcome_patcourse_admit == '(Unknown)' ~ levels_ynu[3],
        TRUE ~ levels_ynu[2]
      ) %>% factor(levels = levels_ynu),
      merge_oxygen = combine_care(MSF_received_oxygen, MSF_outcome_received_oxygen),
      merge_icu    = combine_care(patcourse_icu , outcome_patcourse_icu),
      merge_vent   = combine_care(patcourse_vent, outcome_patcourse_vent)
    ) %>% 
    select(-c(ind_patcourse_admit, ind_outcome_patcourse_admit))
  
  return(dta)
}



#' Combine care
#'
#' Specific function that combine 'twin' variables of patient's care at
#' admission (`var1`) and at discharge (`var2`) in to a single variable. Input
#' variable is coded with values 'Yes' or 'No'. Output variable is code with
#' values 'Yes', 'No at admission then not reported', 'No at any time' or 'Not
#' reported'.
#'
#' @param var1 variable at admission
#' @param var2 variable at discharge
#'
#' @return a character vector
#' @export
#'
combine_care <- function(var1, var2) {
  case_when(
    var1 == 'Yes' | var2 == 'Yes' ~ 'Yes',
    var1 == 'No' & (var2 == 'Unknown' | is.na(var2)) ~ 'No at admission then not reported',
    (var1 == 'No' | is.na(var1)) & var2 == 'No' ~ 'No at any time',
    TRUE ~ 'Not reported'
  ) %>% factor(levels = c('Yes', 'No at admission then not reported', 'No at any time', 'Not reported'))
}



#' Return a date variable set to the Monday of the week
#'
#' Function that return for any day of the week a date set to the Monday of that week.
#'
#' @param date a date
#'
#' @return
#' @export
#'
monday_week_date <- function(date) {
  lubridate::wday(date, week_start = 1) <- 1
  return(date)
}



#' Label breaks
#'
#' Create two-sided labels from a sequence of values
#'
#' @param breaks a vector with break values (`Inf` may be included)
#' @param exclusive logical, indicating whether the right side of the label should display a mutually exclusive value with the left side of the following label. If FALSE (default), the labels display not mutually exclusive values (i.e. "1-10", 10-100", etc.). If TRUE the labels display mutually exclusive values (i.e. "1-9", 10-99", etc.).
#' @param add_Inf logical, if TRUE (default), and if not already present, the value `Inf` is added as last value. If TRUE the length of the output vector is of the same input vector. If FALSE the length of the output vector is the length of the input vector -1.
#' @param replace_Inf logical, if TRUE (default) the value `Inf` is replaced by a +.
#'
#' @return a vector of the same length of the input vector of length of the input vector - 1 if `add_Inf` is set as FALSE.
#' @export
#'
#' @examples
#' x <- c(1, 10, 100, Inf)
#' label_breaks(x)
#' label_breaks(x, exclusive = TRUE)
#' label_breaks(x, exclusive = FALSE, replace_Inf = TRUE)
#'
#' y <- c(1, 10, 100)
#' label_breaks(y)
#' label_breaks(y, exclusive = TRUE) # same length of input vector
#' label_breaks(y, exclusive = FALSE, add_Inf = FALSE) # length of input vector - 1
label_breaks <- function(breaks, exclusive = FALSE, add_Inf = TRUE, replace_Inf = TRUE) {
  
  if (is.unsorted(breaks, strictly = TRUE))
    stop('The values need to be in strict increasing order.')
  
  if (add_Inf && !any(is.infinite(breaks))) {
    breaks[length(breaks) + 1] <- Inf
  }
  
  if (exclusive){
    lbls <- sprintf("%s-%s", prettyNum(breaks[-length(breaks)], big.mark = ' '), prettyNum(breaks[-1] - 1, big.mark = ' '))
  } else {
    lbls <- sprintf("%s-%s", scales::label_number_si()(breaks[1:length(breaks) - 1]), scales::label_number_si()(breaks[2:length(breaks)]))
  }
  
  if(replace_Inf){
    lbls <- gsub("-Inf", "+", lbls)
  }
  
  return(lbls)
}
