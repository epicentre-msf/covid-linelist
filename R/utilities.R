
bind_query_list <- function(...,
                            .id = "query_id",
                            cols_arrange = c("query_id",
                                             "site",
                                             "MSF_N_Patient",
                                             "linelist_row")) {
  out <- dplyr::bind_rows(..., .id = .id)
  dplyr::arrange(out, dplyr::across(all_of(cols_arrange)))
}


write_by_country <- function(x, dat, path_prefix = "local/ll_covid_raw_") {
  dat_write <- dat[dat$country == x,]
  path_write <- paste0(path_prefix, x, ".rds")
  saveRDS(dat_write, path_write)
}


is_excel_numeric <- function(x) {
  # check whether character string is probable excel date
  # i.e. string consisting solely of 4-6 digits
  grepl("^[[:digit:]]{4,6}$", x)
}


parse_excel_dates <- function(x) {
  # parse character string consisting of R date (YYYY-MM-DD) OR excel date
  #  (NNNNN); output will be character in "YYYY-MM-DD" format
  x <- as.character(x)
  i <- is_excel_numeric(x)
  x[i] <- as.character(janitor::excel_numeric_to_date(as_integer_quiet(x[i])))
  x
}


parse_other_dates <- function(x, order = c("%d/%m/%Y", "%Y-%m-%d")) {
  x <- as.character(x)
  i <- grepl("\\/", x)
  x[i] <- as.character(lubridate::parse_date_time(x[i], order = order))
  x
}


parse_dates <- function(x) {
  x <- as.character(x)
  x <- parse_excel_dates(x)
  x <- parse_other_dates(x)
  suppressWarnings(lubridate::as_date(x))
}

non_valid_date <- function(x) {
  x_date <- parse_dates(x)
  !is.na(x) & is.na(x_date)
}


min_safe <- function(x) {
  if (all(is.na(x))) {
    vctrs::vec_cast(NA, x)
  } else {
    min(x, na.rm = TRUE)
  }
}


max_safe <- function(x) {
  if (all(is.na(x))) {
    vctrs::vec_cast(NA, x)
  } else {
    max(x, na.rm = TRUE)
  }
}


get_site_meta <- function(file_path) {
  
  df <- readxlsb::read_xlsb(
    file_path,
    sheet = "options",
    col_types = "string"
  )
  
  language <- df$value[df$variable == "language"]
  version <- df$value[df$variable == "version"]
  
  return(tibble::tibble(linelist_lang = language, linelist_vers = version))
}


bind_queries <- function(..., col_arrange = c("country", "site", "MSF_N_Patient")) {
  # requires dplyr >= 1.0
  # library(dplyr)
  out <- dplyr::bind_rows(...)
  arrange(out, dplyr::across(all_of(col_arrange)))
}


valid_numeric <- function(x) {
  xnum <- suppressWarnings(as.numeric(x))
  is.na(x) | (!is.na(x) & !is.na(xnum))
}



test_duplicated <- function(x, name) {
  dups <- duplicated(x)
  if (any(dups)) {
    warning(sprintf("The following values of %s are duplicated: ", name),
            concat_out(x[dups]), call. = FALSE)
  }
}


concat_out <- function(x) {
  paste0("c(", paste(dQuote(x, q = FALSE), collapse = ", "), ")")
}


check_files_to_dict <- function(path) {
  
  library(readxl)
  library(dplyr)
  
  out <- read_xlsx(path, col_types = "text") %>% 
    filter(!is.na(value), !is.na(date_correct)) %>%
    mutate(ignore = ifelse(date_correct == ".ignore", TRUE, FALSE),
           date_correct = ifelse(date_correct == ".ignore", NA_character_, date_correct),
           date = ifelse(ignore, as.character(value), as.character(date_correct)),   # overwrite date with date_corrected
           date = ifelse(date == ".na", NA_character_, date),
           date = vapply(date, parse_excel_dates, ""),
           date = as.Date(date)) %>%
    dplyr::select(patient_id, variable, value, date, flag, ignore)
  
  return(out)
}


create_empty_dict_dates <- function() {
  tibble::tibble(
    patient_id = character(0),
    variable = character(0),
    value = character(0),
    date = as.Date(character(0)),
    flag = character(0),
    ignore = logical(0)
  )
}


fetch_georef <- function(iso, path = path_shapefiles) {
  readRDS(file.path(path_shapefiles, iso, glue::glue("adm_reference_{iso}.rds")))
}


is_date <- function(x) {
  class(x) == "Date"
}


date_format <- function(x) {
  xx <- as.character(x)
  xx[is.na(xx)] <- "NA"
  return(xx)
}


format_text <- function(x) {
  library(stringr)
  xx <- toupper(x)
  xx <- str_trim(xx)
  # xx <- str_replace_all(xx, "[^[:alnum:]]+", "_")
  # xx <- str_replace(xx, "[_]+\\b", "")
  xx <- base::iconv(xx, to = "ASCII//TRANSLIT")
  xx <- str_replace_all(xx, "[^[[:alnum:]]]+", "")
  xx[xx == ""] <- NA_character_
  return(xx)
}


best_geolevel <- function(dat, pattern) {
  cols <- grep(pattern, names(dat), value = TRUE)
  apply(dat[, sort(cols), drop = FALSE], 1, max_not_na)
}


max_not_na <- function(x, no_na = 0L, value = FALSE) {
  ifelse(any(!is.na(x)), max(which(!is.na(x))), no_na)
}


dist_match <- function(x, y, max_dist = 1) {
  abs(as.numeric(x - y)) <= max_dist | is.na(x) | is.na(y)
}


equal_or_na <- function(x, y) {
  x == y | is.na(x) | is.na(y)
}


time_stamp <- function(t = Sys.time(), format = "%Y-%m-%d_%H%M") {
  # convenience function for creating time stamps
  format(t, format = format)
}


is_posix <- function(x) {
  # test whether object 'x' is of class POSIXt/POSIXct/POSIXlt
  any(class(x) %in% c("POSIXt", "POSIXct", "POSIXlt"))
}


list_files <- function(path = ".", pattern = NULL, full.names = FALSE,
                       ignore.case = FALSE, ignore.temp = TRUE,
                       last.sorted = FALSE) {
  # convenience version of list.files to find files within a directory while
  #  ignoring temporary files (which on some systems begin with "~$")
  # if pattern matches multiple files differing only in their time stamps, arg
  #  last.sorted allows user to retain only the most recent file
  files <- list.files(path = path, pattern = pattern)
  if (ignore.temp) {
    # remove temporary files (negative look-behind for "~$" before 'pattern')
    files <- grep(paste0("(?<!\\~\\$)", pattern), files,
                  ignore.case = ignore.case, value = TRUE, perl = TRUE)
  }
  if (last.sorted) {
    # select only the last-sorted file
    files <- tail(sort(files), 1)
  }
  if (full.names) {
    files <- file.path(path, files)
  }
  files
}


list_dirs <- function(path = ".", pattern = NULL) {
  x <- list.dirs(path)
  if (!is.null(pattern)) x <- x[grepl(pattern, x)]
  x
}


left_join_replace <- function(x, y, cols_match) {
  # add columns in y to x, by = cols_match, overwriting any common columns
  library(dplyr)
  
  cols_orig_x <- names(x)
  cols_replace <- setdiff(names(y), cols_match)
  cols_keep <- setdiff(names(x), cols_replace)
  
  x %>%
    dplyr::select(!!!cols_keep) %>% 
    dplyr::left_join(y, by = cols_match) %>% 
    select(all_of(cols_orig_x))
}


integer_id <- function(x) {
  # convert character or factor vector 'x' to integer ids
  as.integer(factor(x, levels = unique(x)))
}


as_numeric_quiet <- function(x) {
  # as.numeric without coerce warnings
  suppressWarnings(as.numeric(x))
}


as_integer_quiet <- function(x) {
  # as.integer without coerce warnings
  suppressWarnings(as.integer(x))
}


get_date_differences <- function(x, col) {
  
  dat_diff <- mutate_all(x, as.integer) - as.integer(x[[col]])
  
  dat_diff %>% 
    as_tibble() %>% 
    gather("variable", "value") %>% 
    mutate(variable_focal = col, .before = 2,
           variable_label = paste0(variable, " - ", variable_focal)) %>% 
  filter(variable != variable_focal)
}


write_query_tracker <- function(queries_out, site_focal = NULL, path) {
  
  if (!is.null(site_focal)) {
    queries_out <- queries_out %>% 
      filter(site == site_focal)
  }
  
  queries_summary <- queries_out %>% 
    group_by(category, query_id, description) %>% 
    summarize(n_total = n(), .groups = "drop") %>% 
    arrange(desc(n_total)) %>% 
    select(Category = category, `Query ID` = query_id, `N (Total)` = n_total, Description = description)
  
  header_recode <- c(
    "i" = "i",
    "Query group" = "query_group",
    "Query #" = "query_number",
    "OC" = "OC",
    "Country" = "country",
    "Site" = "site",
    "Upload date" = "upload_date",
    "# Patient" = "MSF_N_Patient",
    "Row (export)" = "linelist_row",
    "Query category" = "category",
    "Query ID" = "query_id",
    "Description" = "description",
    "Variable 1" = "variable1",
    "Value 1" = "value1",
    "Variable 2" = "variable2",
    "Value 2" = "value2",
    "Date query generated" = "date_generated",
    "Resolved (auto)" = "resolved_auto",
    "Date resolved (auto)" = "date_resolved_auto",
    "Resolved (field)" = "resolved_field",
    "Date resolved (field)" = "date_resolved_field",
    "Comment (field)" = "comment"
  )
  
  queries_out <- queries_out %>% 
    rename(!!!header_recode)
  
  ## Write updated query tracker sheets to file
  library(openxlsx)
  options("openxlsx.dateFormat" = "yyyy-mm-dd")
  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "Queries", zoom = 130)
  hs <- openxlsx::createStyle(halign = "center", textDecoration = "Bold")
  
  # sheet 1 (Main query tracker)
  openxlsx::writeData(wb, 1, queries_out, withFilter = TRUE)
  openxlsx::setColWidths(wb, 1, cols = 1:ncol(queries_out),
                         widths = c(6, 15, 14, 10, 12, 13, 15, 20, 15, 20, 15, 100, 40, 30, 40, 30, 22, 17, 22, 17, 22, 70))
  openxlsx::freezePane(wb, 1, firstActiveRow = 2)
  openxlsx::addStyle(wb, 1, style = hs, rows = 1, cols = 1:ncol(queries_out), gridExpand = TRUE)
  openxlsx::conditionalFormatting(wb, 1,
                                  cols = 1:ncol(queries_out),
                                  rows = 2:(nrow(queries_out) + 1L),
                                  rule = paste0("$A2>0"),
                                  style = openxlsx::createStyle(bgFill = "#fddbc7"))
  
  # sheet 2 (summary)
  openxlsx::addWorksheet(wb, "Summary", zoom = 130)
  openxlsx::writeData(wb, 2, queries_summary)
  openxlsx::freezePane(wb, 2, firstActiveRow = 2)
  openxlsx::addStyle(wb, 2, style = hs, rows = 1, cols = 1:ncol(queries_summary), gridExpand = TRUE)
  openxlsx::setColWidths(wb, 2, cols = 1:ncol(queries_summary),
                         widths = c(21, 14, 10, 110))
  
  suppressMessages(
    openxlsx::saveWorkbook(
      wb,
      file = path,
      overwrite = TRUE
    )
  )
}


write_query_tracker_site <- function(queries_out) {
  
  paths_oc_site <- distinct(queries_out, OC, site) %>% 
    arrange(OC, site) %>% 
    mutate(path_site = file.path(path_export_fp, OC, "queries", site),
           path_file = file.path(path_site, glue("query_tracker_{site}_{today()}.xlsx")))
  
  for (i in seq_len(nrow(paths_oc_site))) {
    if (!dir.exists(paths_oc_site$path_site[i])) {
      dir.create(paths_oc_site$path_site[i])
    }
    
    write_query_tracker(queries_out,
                        site_focal = paths_oc_site$site[i],
                        path = paths_oc_site$path_file[i])
  }
}

