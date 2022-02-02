
get_os_root <- function() {
  OS <- Sys.info()[['sysname']]
  dplyr::case_when(
    OS == "Windows" ~ "D:",
    OS == "Darwin" ~ "~"
  )
}

get_sharepoint_path <- function() {
  fs::path(get_os_root(), "MSF", "GRP-EPI-COVID-19 - NCoVEpi")
}

get_prev_sunday <- function(date) {
  lubridate::floor_date(as.Date(date), unit = "week", week_start = 7)
}

pad_number <- function(x) {
  formatC(as.numeric(x), width = 2, format = "d", flag = "0")
}

get_linelist_data <- function(shrpnt_path, date_max) {
  path <- max(fs::dir_ls(file.path(shrpnt_path, "data", "linelist", "world"), regexp = "[.]rds$"))
  
  age_cut <- c(0, 5, 10, seq(15, 75, 10), Inf)
  age_labs <- c("0-4", "5-9", "10-14", "15-24", "25-34", "35-44", "45-54", "55-64", "65-74", "75+")
  
  df_linelist <- readr::read_rds(path) %>% 
    mutate_at(vars(contains("date")), lubridate::as_date) %>% 
    mutate(epi_week_event = lubridate::floor_date(date_event, unit = "week", week_start = 1), .after = date_event) %>%
    mutate(
      patcourse_presHCF = lubridate::as_date(patcourse_presHCF),
      outcome_patcourse_presHCF = lubridate::as_date(outcome_patcourse_presHCF),
      delay_before_consultation = MSF_date_consultation - patcourse_dateonset,
      date_master = coalesce(MSF_date_consultation, report_date, Lab_date1, outcome_date_of_outcome, upload_date),
      continent = countrycode::countrycode(country, "iso3c", "continent"),
      region = countrycode::countrycode(country, "iso3c", "region23"),
      country_lab = countrycode::countrycode(country, "iso3c", "country.name"),
      country_lab = dplyr::case_when(
        shape == "BGD_Camp" ~ "Bangladesh Camp",
        country_lab == "Congo - Kinshasa" ~ "Democratic Republic of the Congo", 
        country_lab == "Congo - Brazzaville" ~ "Republic of Congo",
        TRUE ~ country_lab
      ),
      age_group = cut(age_in_years, breaks = age_cut, labels = age_labs, right = FALSE, include.lowest = TRUE),
      age_group = forcats::fct_explicit_na(age_group, na_level = "(Unknown)")
    ) %>% 
    filter(date_master >= as.Date("2020-01-01") & date_master <= date_max | is.na(date_master)) %>% 
    tidyr::replace_na(list(
      patinfo_sex = "(Unknown)",
      MSF_visit_type = "(Unknown)",
      MSF_severity = "(Unknown)",
      triage_site = "No"
    )) %>% 
    select(1:3, continent, region, country_lab, everything())
  
  df_linelist
}

get_agg_data <- function(shrpnt_path, date_max) {
  path_agg_data <- file.path(shrpnt_path, "coordination", "Surveillance focal points coordination", "Aggregated reporting", "Report_covid_aggregate_all_v2.xlsx")
  
  agg_data_names <- c("sheet", "OC", "country_lab", "project", "date", "week", "Suspected", "Probable", "Confirmed", "Not a case", "Unknown")
  
  agg_sheets <- excel_sheets(path_agg_data)
  agg_sheets <- agg_sheets[!grepl("sheet", agg_sheets, ignore.case = TRUE)]
  
  df_weekly_aggregated <- agg_sheets %>% 
    setdiff(., c("ECU- Quito", "BGD - BD102")) %>% 
    map_df(~{
      print(.x)
      oc      <- read_excel(path = path_agg_data, sheet = .x, range = "B1", col_names = FALSE) %>% pull()
      country <- read_excel(path = path_agg_data, sheet = .x, range = "D1", col_names = FALSE) %>% pull()
      project <- read_excel(path = path_agg_data, sheet = .x, range = "F1", col_names = FALSE) %>% pull()
      
      df <- read_excel(path = path_agg_data, sheet = .x, skip = 5, col_names = FALSE)
      if(nrow(df) < 1) return(NULL)
      
      last_col <- ifelse(ncol(df) < 7, ncol(df), 7)
      df %>% 
        mutate(sheet = .x, OC = oc, country_lab = country, project = project) %>% 
        select(sheet, OC, country_lab, project, 1:last_col)
      
    }) %>% 
    set_names(agg_data_names) %>% 
    tidyr::separate(sheet, into = c("country", "site_name"), sep = "-", extra = "merge") %>% 
    mutate(
      country = stringr::str_trim(country), 
      country = case_when(country == "RCA" ~ "CAF", TRUE ~ country),
      site_name = paste(stringr::str_trim(site_name), "(*)"), 
      continent = countrycode::countrycode(country, "iso3c", "continent"),
      region = countrycode::countrycode(country, "iso3c", "region23"),
      country_lab = countrycode::countrycode(country, "iso3c", "country.name"),
      country_lab = dplyr::case_when(
        country_lab == "Congo - Kinshasa" ~ "Democratic Republic of the Congo", 
        country_lab == "Congo - Brazzaville" ~ "Republic of Congo",
        TRUE ~ country_lab),
      date = lubridate::as_date(date),
      Total = purrr::pmap_dbl(list(Suspected, Probable, Confirmed, `Not a case`, Unknown), sum, na.rm = TRUE),
      data_source = "Aggregated"
    ) %>% 
    filter(date <= date_max) %>% 
    select(continent, region, country, country_lab, site_name, project, everything())
  
  df_weekly_aggregated
}

get_ocba_agg_data <- function(shrpnt_path, date_max) {
  
  ocba_dir <- fs::path(shrpnt_path, "coordination", "Surveillance focal points coordination", "Aggregated reporting", "OCBA")
  
  data_dict <- readr::read_csv(fs::path(ocba_dir, "Dict_OU.csv"), show_col_types = FALSE) %>%
    janitor::clean_names() 
  # data_dict <- read_csv("data-raw/ocba_dict.csv") %>% janitor::remove_empty("cols") 
  # write_csv(data_dict, path(ocba_dir, "Dict_OU.csv"))
  
  latest_data_path <- max(list.files(ocba_dir, pattern = "^COVID19_AG", full.names = TRUE))
  
  df_agg_ocba <- readr::read_csv(latest_data_path, show_col_types = FALSE) %>% 
    janitor::clean_names() %>% 
    mutate(
      status = case_when(
        str_detect(data, "confirmed") ~ "Confirmed",
        str_detect(data, "suspected") ~ "Suspected",
        TRUE ~ "Unknown"
      )
    ) %>% 
    add_count(period, data, organisation_unit, wt = value, name = "Total") %>% 
    pivot_wider(names_from = "status", values_from = "value", values_fill = 0) %>% 
    filter(Total > 0) %>% 
    mutate(
      year = str_extract(period, "\\d{4}"),
      week = str_extract(period, "W\\d+") %>% parse_number(),
      date = aweek::week2date(glue::glue("{year}-W{pad_number(week)}")),
      .before = 1
    ) %>% 
    select(-period, -year, -week, -data) %>% 
    rename(site_name = organisation_unit) %>% 
    left_join(select(data_dict, country = country_code_iso3, site_name = organisation_unit_level_4), by = "site_name") %>% 
    mutate(
      continent = countrycode::countrycode(country, "iso3c", "continent"),
      # flag = countrycode::countrycode(country, "iso3c", "unicode.symbol"),
      country_lab = countrycode::countrycode(country, "iso3c", "country.name"),
      country_lab = dplyr::case_when(
        country_lab == "Congo - Kinshasa" ~ "Democratic Republic of the Congo", 
        country_lab == "Congo - Brazzaville" ~ "Republic of Congo",
        TRUE ~ country_lab
      ),
      OC = "OCBA",
      site_name = paste(stringr::str_trim(site_name), "(*)"), 
      .before = 1
    ) %>% 
    mutate(data_source = "Aggregated") %>% 
    filter(date <= date_max) %>% 
    relocate(country, .before = country_lab) 
  
  df_agg_ocba
}





