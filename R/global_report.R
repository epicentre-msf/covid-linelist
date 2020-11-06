
### Required libraries, paths, and global dictionaries
source("R/zzz.R")
library(tidyverse)
library(llutils)
library(readxl)
library(janitor)
library(stringr)
library(lubridate)
library(openxlsx)



### read latest MSF ll compilation
d_ll <- llutils::list_files(
  path_export_global,
  pattern = "\\.rds",
  full.names = TRUE,
  select = "latest"
) %>% 
  readRDS()

# assorted checks
# d_ll %>% 
#   mutate(has_Lab_date1 = !is.na(Lab_date1),
#          has_MSF_sample_type = !is.na(MSF_sample_type),
#          has_MSF_test_type = !is.na(MSF_test_type)) %>% 
#   count(has_Lab_date1, has_MSF_sample_type, has_MSF_test_type, MSF_test_results)
# 
# 
# d_ll %>% 
#   select(Lab_date1, MSF_test_type, MSF_test_results, outcome_lab_date, outcome_lab_result) %>% 
#   mutate(same_date = Lab_date1 == outcome_lab_date) %>% 
#   filter(same_date)
# 
# d_ll %>% 
#   select(site, Lab_date1, MSF_test_type, MSF_test_results, outcome_lab_date, outcome_lab_result) %>% 
#   mutate(same_date = Lab_date1 == outcome_lab_date) %>% 
#   filter(!is.na(outcome_lab_date) & is.na(Lab_date1)) %>% 
#   filter(!is.na(MSF_test_type) | !is.na(MSF_test_results))
# 
# d_ll %>% 
#   select(site, Lab_date1, MSF_test_type, MSF_test_results, outcome_lab_date, outcome_lab_result) %>% 
#   mutate(same_date = Lab_date1 == outcome_lab_date) %>% 
#   filter(!is.na(Lab_date1) & is.na(outcome_lab_date) & !is.na(outcome_lab_result)) %>% 
#   filter(!outcome_lab_result %in% c("Not done", "Negative"))
  


# number consultations (not a case/suspected/confirmed/probable)
# by inpatient, outpatient, unclear
type_admit <- c(
  "First hospitalisation", 
  "First hospitalisation after a consultation",
  "Rehospitalisation",
  "Admission to isolation center"
)

d_ll_prep <- d_ll %>% 
  # filter to covid status suspect+
  filter(!MSF_covid_status %in% c("Not a suspect", NA)) %>% 
  # derive visit_type
  mutate(
    visit_type = case_when(
      MSF_visit_type %in% c("First consultation") ~ "OPD",
      MSF_visit_type %in% type_admit ~ "IPD",
      TRUE ~ "Unknown"
    ),
    # derive severity
    severe = case_when(
      MSF_received_oxygen == "Yes" |
        MSF_outcome_received_oxygen == "Yes" |
        patcourse_vent == "Yes" |
        outcome_patcourse_vent == "Yes" |
        patcourse_icu == "Yes" |
        outcome_patcourse_icu == "Yes" ~ "Yes",
      TRUE ~ "No"
    ),
    # derive tested
    tested = case_when(
      !is.na(Lab_date1) | !is.na(outcome_lab_date) |
        MSF_test_type %in% c("PCR", "RDT", "X-ray", "CT scan", "RDT antibody", "RDT antigen") ~ "Yes",
      TRUE ~ "No"
    ),
    # derive month
    date = coalesce(MSF_date_consultation, report_date, Lab_date1, outcome_date_of_outcome, upload_date),
    year = format(date, "%Y"),
    month = format(date, "%B"),
    month_int = as.integer(format(date, "%m"))
  )


# d_ll_prep %>% 
#   mutate(has_lab_date = ifelse(is.na(Lab_date1), "No", "Yes")) %>% 
#   mutate(has_outcome_lab_date = ifelse(is.na(outcome_lab_date), "No", "Yes")) %>% 
#   count(has_lab_date, has_outcome_lab_date, MSF_test_type, MSF_test_results, outcome_lab_result)
#   
# d_ll_prep %>% 
#   mutate(has_lab_date = ifelse(is.na(Lab_date1), "No", "Yes")) %>% 
#   mutate(has_outcome_lab_date = ifelse(is.na(outcome_lab_date), "No", "Yes")) %>% 
#   count(has_lab_date, has_outcome_lab_date, MSF_test_type, MSF_test_results, outcome_lab_result)



# summarize n_visits, n_confirmed, etc. from MLL linelist compilation
ll_counts <- d_ll_prep %>% 
  mutate(project = ifelse(site == "YEM_P_HAY", "YE112", project)) %>%
  group_by(country, OC, project, site_code = site, site_name, year, month_int, month) %>% 
  summarize(
    n_visits_unknown_type = sum(!visit_type %in% c("OPD", "IPD")),
    n_visits_opd = sum(visit_type == "OPD"),
    n_visits_ipd = sum(visit_type == "IPD"),
    n_confirmed = sum(MSF_covid_status == "Confirmed"),
    n_severe = sum(severe == "Yes"),
    n_died = sum(outcome_patcourse_status == "Died"),
    n_tested = sum(tested == "Yes"),
    .groups = "drop"
  ) %>% 
  mutate(
    across(starts_with("n_"), ~ as.integer(ifelse(is.na(.x), 0, .x))),
    data_source = "linelist"
  )



### read aggregated data file
read_aggregate <- function(path, sheet) {
  
  cols <- c("oc", "country_lab", "country", "project_lab", "project")
  df_meta <- readxl::read_excel(path, sheet = sheet, range = "B1:F1", col_names = cols)
  
  d <- readxl::read_excel(path, sheet = sheet, skip = 3) %>% 
    janitor::clean_names() %>% 
    mutate(sheet = sheet, OC = df_meta$oc, country_lab = df_meta$country, project = df_meta$project)
  
  d_total <- dplyr::slice(d, 1)
  
  has_total <- !is.na(d_total$suspected_cases) |
    !is.na(d_total$probable_cases) | 
    !is.na(d_total$confirmed_cases) |
    !is.na(d_total$unknown_status) |
    !is.na(d_total$number_of_msf_supported_c19_suspect_opd_consultations)
  
  d_minus_total <- dplyr::slice(d, -1)
  
  has_weekly <- any(
    !is.na(d_minus_total$suspected_cases) |
      !is.na(d_minus_total$probable_cases) | 
      !is.na(d_minus_total$confirmed_cases) |
      !is.na(d_minus_total$unknown_status) |
      !is.na(d_minus_total$number_of_msf_supported_c19_suspect_opd_consultations)
  )
  
  if (has_total & !has_weekly) {
    out <- d_total
  } else {
    out <- d_minus_total
  }
  
  out
}

aggregated_counts_raw <- readxl::excel_sheets(path_aggregate_data) %>% 
  grep("^Sheet|^Feuil", ., value = TRUE, ignore.case = TRUE, invert = TRUE) %>% 
  purrr::map_dfr(., read_aggregate, path = path_aggregate_data)

aggregated_counts <- aggregated_counts_raw %>% 
  rename("date" = "x1st_day_of_the_week_indicative",
         "n_visits_opd" = "number_of_msf_supported_c19_suspect_opd_consultations",
         "n_visits_ipd" = "number_of_msf_supported_c19_suspect_and_confirmed_ipd_consultations",
         "n_severe" = "number_of_msf_treated_severe_patients_o2",
         "n_died" = "number_of_c19_deaths_in_msf_supported_facilities",
         "n_tested" = "number_number_of_c19_tests_performed") %>% 
  select(-x13) %>% 
  mutate(
    country = stringr::str_sub(sheet, 1, 3),
    site_name = gsub("^[[:alpha:]]{3}[[:space:]]*\\-[[:space:]]*", "", sheet),
    country = stringr::str_trim(country), 
    site_name = stringr::str_trim(site_name), 
    date = lubridate::as_date(date),
    n_visits_unknown_type = purrr::pmap_dbl(list(suspected_cases, probable_cases, confirmed_cases, non_cases, unknown_status), sum, na.rm = TRUE),
    n_visits_unknown_type = ifelse(is.na(n_visits_opd) & is.na(n_visits_ipd), n_visits_unknown_type, NA),
    year = format(date, "%Y"),
    month = format(date, "%B"),
    month_int = as.integer(format(date, "%m"))
  ) %>% 
  group_by(country, OC, project, site_name, year, month_int, month) %>% 
  summarize(
    n_visits_unknown_type = sum(n_visits_unknown_type, na.rm = TRUE),
    n_visits_opd = sum(n_visits_opd, na.rm = TRUE),
    n_visits_ipd = sum(n_visits_ipd, na.rm = TRUE),
    n_confirmed = sum(confirmed_cases, na.rm = TRUE),
    n_severe = sum(n_severe, na.rm = TRUE),
    n_died = sum(n_died, na.rm = TRUE),
    n_tested = sum(n_tested, na.rm = TRUE),
    .groups = "drop"
  ) %>% 
  mutate(data_source = "aggregated")


output_colnames <- c(
  "Region" = "region",
  "Country code" = "country_code",
  "Country name" = 'country_name',
  "OC" = "OC",
  "Project" = "project",
  "Site code (linelist)" = "site_code",
  "Site name" = "site_name",
  "Data source" = "data_source",
  # "Year" = "year",
  # "Month" = "month",
  "Month" = "month_year",
  "# MSF-supported C19 visits (unknown visit type)" = "n_visits_unknown_type",
  "# MSF-supported C19 OPD visits" = "n_visits_opd",
  "# MSF-supported C19 IPD visits" = "n_visits_ipd",
  "# MSF-supported C19 Confirmed patients" = "n_confirmed",
  "# of MSF-treated severe patients" = "n_severe",
  "# of C19 deaths in MSF-supported facilities" = "n_died",
  "# of patients tested for C19" = "n_tested"
)

output_colnames_rev <- setNames(
  names(output_colnames),
  output_colnames
)

months_fact <- c(
  "February 2020",
  "March 2020",
  "April 2020",
  "May 2020",
  "June 2020",
  "July 2020",
  "August 2020",
  "TOTAL"
)


### combine linelist and aggregated counts
counts_full <- dplyr::bind_rows(ll_counts, aggregated_counts) %>% 
  filter(month_int < 9 | is.na(month_int)) %>%
  mutate(
    country = recode(country, "RCA" = "CAF"),
    country_name = countrycode::countrycode(country, origin = "iso3c", destination = "country.name"),
    region = countrycode::countrycode(country, origin = "iso3c", destination = "region23"),
    month_year = paste(month, year),
    month_year = ifelse(month_year == "NA NA", "TOTAL", month_year),
    month_year = factor(month_year, levels = months_fact)
  ) %>% 
  select(-month, -month_int, -year) %>% 
  select(region, country_code = country, country_name, OC, project, site_code, site_name, data_source, month_year, everything()) %>% 
  arrange(region, country_name, OC, site_name) %>% 
  rename(!!output_colnames)

# sanity checks
setdiff(counts_full$`Site code (linelist)`, d_ll$site)
setdiff(d_ll$site, counts_full$`Site code (linelist)`)

# write file
wb <- llutils::write_simple_xlsx(counts_full)
hs <- openxlsx::createStyle(halign = "center", textDecoration = "Bold", wrapText = TRUE)

openxlsx::addStyle(
  wb,
  1,
  style = hs,
  rows = 1,
  cols = 1:ncol(counts_full),
  gridExpand = TRUE
)

openxlsx::setRowHeights(
  wb,
  sheet = 1,
  rows = 1,
  heights = 46
)

openxlsx::setColWidths(
  wb,
  sheet = 1,
  cols = 1:ncol(counts_full),
  widths = c(20, 12, 25, 10, 16, 15, 35, 12, 12, 20, 20, 20, 20, 20, 20)
)

suppressMessages(
  openxlsx::saveWorkbook(
    wb,
    file = file.path(path_global_report, glue::glue("msf_covid19_global_report_summary_{Sys.Date()}.xlsx")),
    overwrite = TRUE
  )
)



### country specific counts in relation to total country counts from ECDC
ecdc_raw <- covidutils::get_ecdc_data()

ecdc <- ecdc_raw %>% 
  mutate(
    month = lubridate::month(date, label = TRUE, abbr = FALSE),
    year = lubridate::year(date),
    month_year = paste(month, year)
  ) %>% 
  group_by(country_code = iso_a3, month_year) %>% 
  summarize(
    ecdc_cases = sum(cases, na.rm = TRUE),
    ecdc_deaths = sum(deaths, na.rm = TRUE),
    .groups = "drop"
  )

output_colnames_ecdc <- c(
  "Region" = "region",
  "Country code" = "country_code",
  "Country name" = 'country_name',
  "Month" = "month_year",
  "# MSF-supported C19 visits (all types)" = "n_visits_total",
  "# MSF-supported C19 Confirmed patients" = "n_confirmed",
  "# of C19 deaths in MSF-supported facilities" = "n_died",
  "Total country cases (ECDC)" = "ecdc_cases",
  "Total country deaths (ECDC)" = "ecdc_deaths"
)

global_report_ecdc <- counts_full %>% 
  rename(!!output_colnames_rev) %>% 
  filter(month_year != "TOTAL") %>% 
  group_by(region, country_code, country_name, month_year) %>% 
  summarize(
    n_visits_unknown_type = sum(n_visits_unknown_type),
    n_visits_opd = sum(n_visits_opd),
    n_visits_ipd = sum(n_visits_ipd),
    n_confirmed = sum(n_confirmed),
    n_died = sum(n_died),
    .groups = "drop"
  ) %>% 
  mutate(n_visits_total = n_visits_unknown_type + n_visits_opd + n_visits_ipd, .after = "n_visits_ipd") %>% 
  left_join(ecdc, by = c("country_code", "month_year")) %>% 
  select(-n_visits_unknown_type, -n_visits_opd, -n_visits_ipd) %>% 
  rename(!!any_of(output_colnames_ecdc))
  
  
# llutils::write_simple_xlsx(
#   global_report_ecdc,
#   file.path(path_global_report, glue::glue("msf_covid19_global_report_ECDC_{Sys.Date()}.xlsx"))
# )

