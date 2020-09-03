
### Required libraries
library(tidyverse)
library(readxl)
library(here)
library(glue)
library(lubridate)
source("R/import.R")
source("R/utilities.R")
source("R/zzz.R")


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


### Linelist language and version
sheets <- purrr::map_dfr(
  countries,
  scan_sheets,
  path_data_raw = path_data_raw,
  dict_facilities = dict_facilities,
  return_latest = FALSE
) %>% 
  mutate(meta = map(file_path, get_site_meta)) %>% 
  unnest("meta") %>% 
  mutate(upload_date = as.Date(upload_date)) %>% 
  group_by(site) %>% 
  summarize(
    first_upload = min(upload_date),
    latest_upload = max(upload_date),
    language = unique(linelist_lang),
    version = max(linelist_vers),
    .groups = "drop"
  )


### Proportion admin matchable
dat_summary <- dat %>% 
  group_by(site) %>% 
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
  left_join(sheets, by = "site") %>% 
  left_join(dat_summary, by = "site") %>% 
  arrange(site) %>% 
  mutate(across(where(is.Date), as.character)) %>% 
  select(
    country, OC, project, site_name, site_code = site, site_type, version,
    language, first_upload, latest_upload, geobase_available, visits,
    starts_with("prop"), comment
  )


### Date of most recent Sunday (flag if latest upload before)
cutoff_date <- floor_date(today(), "week")


## Write
library(openxlsx)

wb <- llutils::write_simple_xlsx(out)

red_bg <- createStyle(bgFill = "#FFC7CE")
red_fg <- createStyle(fgFill = "#FFC7CE")

i_no_recent_export <- which(out$latest_upload < cutoff_date) + 1L
addStyle(wb, 1, cols = 10, rows = i_no_recent_export, style = red_fg, stack = TRUE)
addFilter(wb, 1, rows = 1, cols = seq_len(ncol(out)))

conditionalFormatting(wb, 1, cols = 11, rows = 1:nrow(out), type = "contains", rule = "No", style = red_bg)

conditionalFormatting(wb, 1, cols = 13, rows = 1:nrow(out), rule = "<0.50", style = red_bg)
conditionalFormatting(wb, 1, cols = 14, rows = 1:nrow(out), rule = "<0.50", style = red_bg)
conditionalFormatting(wb, 1, cols = 15, rows = 1:nrow(out), rule = "<0.50", style = red_bg)

file_out <- file.path(
  path_implementation,
  sprintf("site_implementation_summary_%s.xlsx", lubridate::today())
)

openxlsx::saveWorkbook(wb, file = file_out, overwrite = TRUE)

