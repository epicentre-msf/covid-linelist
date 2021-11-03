# this script will create a local SQLite database with the linelist
# compilation and aggregated data that is then pushed to the dashboard
# server via rsync over ssh. your ssh public key is required to be in
# .ssh/authorized_keys for both the root and epicentre user on the server
# Patrick - your key is in both so it should work!

library(DBI)
library(tidyverse)
library(fs)
library(readxl)
source(here::here("R", "push_to_server_utils.R"))

shrpnt_path <- get_sharepoint_path()
date_max <- get_prev_sunday(Sys.Date())
data_dir <- here::here("local")
if (!dir.exists(data_dir)) dir.create(data_dir)

# Create a fresh local db, delete only one if exists
db_file <- here::here("local", "linelist.db")
if (file.exists(db_file)) file.remove(db_file)
con <- DBI::dbConnect(RSQLite::SQLite(), db_file)

# import latest linelist compilation
df_linelist <- get_linelist_data(shrpnt_path, date_max)

# write to DB
dbWriteTable(con, "linelist", df_linelist, overwrite = TRUE)

# Get weekly aggregated data
df_weekly_aggregated <- get_agg_data(shrpnt_path, date_max)
df_weekly_aggregated_ocba <- get_ocba_agg_data(shrpnt_path, date_max)

# combine aggregated data
df_combined <- df_linelist %>%
  select(continent, country, country_lab, OC, site_name, date = date_master, MSF_covid_status) %>% 
  tidyr::replace_na(list(MSF_covid_status = "Unknown")) %>% 
  mutate(
    date = lubridate::floor_date(lubridate::as_date(date), "week", week_start = 1), # floor date by week to monday
    country_lab = dplyr::case_when(country_lab == "Bangladesh Camp" ~ "Bangladesh", TRUE ~ country_lab)
  ) %>% 
  group_by_all() %>% 
  summarise(n = n(), .groups = "keep") %>% 
  add_tally(wt = n, name = "Total") %>% 
  ungroup() %>% 
  tidyr::spread(MSF_covid_status, n, fill = 0) %>% 
  select(-Total, everything()) %>% 
  mutate(data_source = "Linelist") %>% 
  bind_rows(
    df_weekly_aggregated %>% 
      filter(!project %in% c('Lesvos-Moria Ped'), Total > 0) %>% 
      select(-region, -project, -week) %>% 
      mutate(date = lubridate::as_date(date))
  ) %>% 
  bind_rows(df_weekly_aggregated_ocba) %>% 
  arrange(continent, country_lab, site_name, date)

# write to DB
DBI::dbWriteTable(con, "combined", df_combined, overwrite = TRUE)

# disconnect from DB
# DBI::dbExecute(con, "VACUUM;")
DBI::dbDisconnect(con)

# run this to send data to server
if (FALSE) {
  # send local database to server with rsync 
  host <- "epicentre@vps709766.ovh.net"
  server_path <- "/home/epicentre/covid-linelist-dashboard"
  system2("rsync", args = c(db_file, paste(host, fs::path(server_path, "data"), sep = ":")))

  # remove cached files on server. need to ssh as root for this...
  # if this doesn't work from R you can just do it manually on episerv:
  # first login as root with `ssh root@vps709766.ovh.net`
  # then run `rm -rf /home/epicentre/covid-linelist-dashboard/.rcache/*`
  # then `exit` to logout
  library(ssh)
  session <- ssh_connect(host = "root@vps709766.ovh.net")
  ssh_exec_wait(session, paste0("rm -rf ", server_path, "/.rcache/*"))

  # keep the testing site up to date - don't bother with this part if it's not working from R
  server_test_path <- "/home/epicentre/covid-linelist-dashboard-testing"
  ssh_exec_wait(session, glue::glue("cp {server_path}/data/linelist.db {server_test_path}/data/"))
  ssh_exec_wait(session, glue::glue("cp {server_path}/data/df_trends.rda {server_test_path}/data/"))
  ssh_exec_wait(session, paste0("rm -rf ", server_test_path, "/.rcache/*"))
  ssh_disconnect(session)
}


