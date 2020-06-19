
### Required libraries
library(tidyverse)
library(lubridate)
library(glue)
source("R/queries_defs.R")
source("R/zzz.R")


### Query definition sheet
query_defs <- readxl::read_xlsx(file.path(path_queries, "query_definitions.xlsx")) %>% 
  janitor::clean_names() %>% 
  select(query_id, category, description) %>% 
  mutate(description = gsub("\\\"", "'", description)) %>% 
  filter(!is.na(query_id))

### Compile global linelist of raw imports
dat_raw <- list.files("local", pattern = "^ll_covid_raw", full.names = TRUE) %>%
  lapply(readRDS) %>%
  dplyr::bind_rows() %>%
  mutate(db_row = 1:n())

dat_clean <- list.files("local", pattern = "msf_covid19_linelist", full.names = TRUE) %>%
  lapply(readRDS) %>%
  dplyr::bind_rows() %>%
  mutate(db_row = 1:n()) %>%
  mutate(outcome_onset_symptom = as.Date(outcome_onset_symptom))



### Run all queries
df_queries <- dplyr::bind_rows(
  queries_ident(dat_raw, dat_clean),
  queries_dates(dat_raw, date_vars, dict_date_categories),
  queries_categorical(dat_raw, dict_factors, dict_countries),
  queries_multi(dat_raw, dat_clean),
  queries_other(dat_raw, dat_clean)
)



### Assemble output
queries_out <- df_queries %>% 
  left_join(query_defs, by = "query_id") %>% 
  arrange(site, query_id, MSF_N_Patient) %>% 
  group_by(site) %>% 
  mutate(query_group = paste(query_id, MSF_N_Patient),
         query_group = integer_id(query_group),
         query_group = formatC(query_group, width = 5, flag = "0"),
         query_group = paste0("Q", query_group)) %>% 
  ungroup() %>% 
  group_by(site, query_group) %>% 
  mutate(query_number = formatC(1:n(), width = 3, flag = "0"),
         query_number = paste(query_group, query_number, sep = "_")) %>% 
  ungroup() %>% 
  select(query_group, query_number, OC, country, site, upload_date, MSF_N_Patient,
         linelist_row, category, query_id, description, everything()) %>% 
  arrange(site, query_number) %>% 
  mutate(i = integer_id(paste(site, query_group)) %% 2, .before = 1) %>% 
  mutate(date_generated = as.character(lubridate::today()),
         resolved_auto = "No",
         date_resolved_auto = NA_character_,
         resolved_field = NA_character_,
         date_resolved_field = NA_character_,
         comment = NA_character_)


nrow(queries_out)
length(unique(paste(queries_out$site, queries_out$query_group)))
queries_out %>% count(query_id, sort = TRUE)
queries_out %>% count(OC, sort = TRUE)
queries_out %>% count(site, sort = TRUE)


### Write
if (FALSE) {
  write_query_tracker_site(queries_out)
  write_query_tracker(queries_out, path = file.path(path_queries, glue("query_tracker_{today()}.xlsx")))
}

