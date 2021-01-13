
### Required libraries
library(tidyverse)
library(readxl)
library(here)
library(glue)
library(lubridate)
source("R/import.R")
source("R/utilities.R")
source("R/zzz.R")


###
### ** Update comments column at each compilation
###


### Read latest global ll
ll_file <- llutils::list_files(
  path_export_global,
  pattern = "msf_covid19_.*\\.rds",
  full.names = TRUE,
  select = "latest"
)

dat <- readRDS(ll_file)


### Check whether geobase ready
geobase <- list.files(path_geobase, pattern = "xlsx") %>% 
  vapply(function(x) strsplit(x, "_[[:digit:]]")[[1]][1], "", USE.NAMES = FALSE) %>% 
  recode("BGD_CAMP" = "BGD_Camp")


### Linelist files
ll_files <- purrr::map_dfr(
  countries,
  scan_sheets,
  path_data_raw = path_data_raw,
  dict_facilities = dict_facilities,
  return_latest = FALSE
)

ll_files_latest <- ll_files %>% 
  group_by(site) %>% 
  filter(upload_date == max(upload_date)) %>% 
  ungroup()


### First upload date by site
first_upload <- ll_files %>% 
  mutate(upload_date = as.Date(upload_date)) %>% 
  group_by(site) %>% 
  summarize(
    first_upload = min(upload_date),
    .groups = "drop"
  )


### Metadata for V3+
site_meta_v3 <- ll_files_latest %>% 
  mutate(meta = map(file_path, get_site_meta)) %>% 
  unnest("meta") %>% 
  select(
    site,
    version = linelist_vers,
    type = linelist_type,
    starts_with("center"),
    starts_with("case_definition")
  ) %>% 
  mutate(across(matches("^center_|^case_def"), ~ dplyr::na_if(.x, "0"))) %>% 
  mutate(across(matches("^center_"), ~ dplyr::recode(.x, "Oui" = "Yes", "Non" = "No")))


### Metadata from compilation
dat_raw <- list.files("local/raw", pattern = "^ll_covid_raw", full.names = TRUE) %>%
  purrr::map_dfr(readRDS) %>% 
  mutate(OC = ifelse(OC == "OCB_&_OCP", "OCB/OCP", OC))

site_meta_comp <- dat_raw %>% 
  mutate(upload_date = parse_dates(upload_date)) %>% 
  group_by(site, version = linelist_vers) %>% 
  summarize(
    latest_upload = max(upload_date),
    language = unique(linelist_lang),
    version = unique(linelist_vers),
    .groups = "drop"
  ) %>% 
  arrange(site, latest_upload)


### Proportion admin matchable
dat_summary <- dat %>% 
  group_by(site, version = ll_version) %>% 
  summarize(
    visits = n(),
    prop_admin1 = sum(!is.na(adm1_name__res)) / visits,
    prop_admin2 = sum(!is.na(adm2_name__res)) / visits,
    prop_admin3 = sum(!is.na(adm3_name__res)) / visits,
    .groups = "drop"
  ) %>% 
  mutate(across(starts_with("prop_admin"), ~ round(.x, digits = 2)))


### Assemble output
out <- dict_facilities %>% 
  mutate(geobase_available = ifelse(shape %in% geobase, "Yes", "No")) %>% 
  select(site, country = country_full, OC, project, site_type, site_name, geobase_available, comment) %>% 
  inner_join(site_meta_comp, by = "site") %>% 
  left_join(first_upload, by = "site") %>% 
  left_join(site_meta_v3, by = c("site", "version")) %>% 
  left_join(dat_summary, by = c("site", "version")) %>% 
  arrange(site) %>% 
  mutate(across(where(is.Date), as.character)) %>% 
  select(
    country, OC, project, site_name, site_code = site, site_type, version, type,
    facility_triage = center_screening,
    facility_OPD = center_consultation,
    facility_IPD = center_admission,
    facility_ICU = center_ICU,
    language,
    first_upload, latest_upload, geobase_available,
    visits, starts_with("prop"),
    comment, starts_with("case_def")
  )

### Date of most recent Sunday (flag if latest upload before)
cutoff_date <- floor_date(today(), "week")


## Write
library(openxlsx)

wb <- llutils::write_simple_xlsx(out)

red_bg <- createStyle(bgFill = "#FFC7CE")
red_fg <- createStyle(fgFill = "#FFC7CE")

i_no_recent_export <- which(out$latest_upload < cutoff_date) + 1L
addStyle(wb, 1, cols = 15, rows = i_no_recent_export, style = red_fg, stack = TRUE)
addStyle(wb, 1, cols = seq_len(ncol(out)), rows = 1, style = createStyle(halign = "left"), stack = TRUE)
addFilter(wb, 1, rows = 1, cols = seq_len(ncol(out)))

conditionalFormatting(wb, 1, cols = 16, rows = 1:nrow(out), type = "contains", rule = "No", style = red_bg)
conditionalFormatting(wb, 1, cols = 18, rows = 1:nrow(out), rule = "<0.50", style = red_bg)
conditionalFormatting(wb, 1, cols = 19, rows = 1:nrow(out), rule = "<0.50", style = red_bg)
conditionalFormatting(wb, 1, cols = 20, rows = 1:nrow(out), rule = "<0.50", style = red_bg)

file_out <- file.path(
  path_implementation,
  sprintf("site_implementation_summary_%s.xlsx", lubridate::today())
)

openxlsx::saveWorkbook(wb, file = file_out, overwrite = TRUE)

