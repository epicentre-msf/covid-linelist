

add_na_dict <- function(d) {
  dplyr::bind_rows(
    d,
    tibble::tibble(values_en = NA, variable_en = unique(d$variable_en))
  )
}


string_to_numeric_prep <- function(x) {
  x %>%
    stringr::str_to_lower() %>%
    stringr::str_replace("[,;/]+", ".") %>%
    # recode(!!!dict_numeric) %>%
    stringr::str_replace("^\\.$", NA_character_)
}


paste_collapse <- function(x, y, collapse = "; ") {
  z <- c(x, y)
  paste(z[!is.na(z)], collapse = collapse)
}

paste_collapse <- Vectorize(paste_collapse, vectorize.args = c("x", "y"),
                            USE.NAMES = FALSE)


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


# test whether each row of data.frame is almost-empty, defined in relation
#  to a critical number of non-<NA> values
almost_empty_rows <- function(df, n_crit = 2, cols_exclude = NULL) {
  df <- df[,!names(df) %in% cols_exclude]
  ncol(df) - rowSums(is.na(df)) < n_crit
}


time_stamp <- function(t = Sys.time(), format = "%Y-%m-%d_%H%M") {
  # convenience function for creating time stamps
  format(t, format = format)
}


date_to_excel_numeric <- function(x) {
  # opposite of janitor::excel_numeric_to_date() (1899-12-30 is date 0)
  as.integer(as.Date(x) - as.Date("1899-12-30"))
}


is_posix <- function(x) {
  # test whether object 'x' is of class POSIXt/POSIXct/POSIXlt
  any(class(x) %in% c("POSIXt", "POSIXct", "POSIXlt"))
}


read_xlsx_and_label <- function(path, name = "file_index", pattern = path) {
  # read xlsx file and add new column based on pattern extracted from path
  library(readxl)
  library(dplyr)
  library(stringr)
  library(rlang)
  out <- readxl::read_xlsx(path)
  out <- dplyr::mutate(out, !!name := stringr::str_extract(path, pattern))
  out
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


left_join_verbose <- function(x, y, by = NULL, copy = FALSE,
                              suffix = c(".x", ".y"), ...) {
  # dplyr::left_join() with warning if any rows of 'x' are duplicated
  library(dplyr)
  x$TEMPORARY_INDEX_COL <- 1:nrow(x)
  out <- dplyr::left_join(x, y, by = by, copy = copy, suffix = suffix, ...)
  if (any(duplicated(out$TEMPORARY_INDEX_COL))) {
    rows_dup <- out$TEMPORARY_INDEX_COL[duplicated(out$TEMPORARY_INDEX_COL)]
    warning("The following rows of 'x' have been duplicated by `left_join()`: ",
            paste(rows_dup, collapse = ", "),
            call. = FALSE)
  }
  out[,names(out) != "TEMPORARY_INDEX_COL"]
}


left_join_replace <- function(x, y, cols_match) {
  # add columns in y to x, by = cols_match, overwriting any common columns
  library(dplyr)
  cols_replace <- setdiff(names(y), cols_match)
  cols_keep <- setdiff(names(x), cols_replace)
  
  x %>%
    dplyr::select(!!!cols_keep) %>% 
    dplyr::left_join(y, by = cols_match)
}


integer_id <- function(x) {
  # convert character or factor vector 'x' to integer ids
  as.integer(factor(x, levels = unique(x)))
}


length_unique <- function(x) {
  # find number of unique values in vector 'x'
  length(unique(x))
}


unique_no_na <- function(x) {
  # return logical indicating whether vector 'x' contains single unique value
  #  with no <NA>
  !any(is.na(x)) && length_unique(x) == 1
}


dates_within_error <- function(x, day_range_max) {
  # takes a vector of dates and determines whether the max difference in days
  #  is less than a given threshold (i.e. whether all the dates are close
  #  together)
  range_in_days <- as.integer(diff(range(as.Date(x))))
  ifelse(range_in_days <= day_range_max, TRUE, FALSE)
}


numeric_within_error <- function(x, range_max) {
  # take a vector of numeric values and determine whether the range (max - min)
  #  is less than a given threshold (i.e. whether all the numbers are close
  #  together)
  numeric_range <- diff(range(x))
  ifelse(numeric_range <= range_max, TRUE, FALSE)
}


is_excel_numeric <- function(x) {
  # check whether character string is probable excel date
  # i.e. string consisting solely of 4-6 digits
  grepl("^[[:digit:]]{4,6}$", x)
}


parse_excel_dates <- function(x) {
  # parse character string consisting of R date (YYYY-MM-DD) OR excel date
  #  (NNNNN); output will be character in "YYYY-MM-DD" format
  library(janitor)
  ifelse(is_excel_numeric(x),
         as.character(janitor::excel_numeric_to_date(as.integer(x))),
         x)
}


as_numeric_quiet <- function(x) {
  # as.numeric without coerce warnings
  suppressWarnings(as.numeric(x))
}



print_and_capture <- function(x) {
  # useful for adding a data.frame to a message() or warning()
  if ("tbl" %in% class(x)) x <- as.data.frame(x)
  paste(utils::capture.output(print(x)), collapse = "\n")
}


## convert age to years
age_to_years_scalar <- function(value, unit) {
  switch(tolower(unit),
         "Year" = value,
         "Month" = value / 12,
         "Day" = value / 365.25,
         value)
}

age_to_years <- function(value, unit) {
  if (!all(tolower(unit) %in% c("year", "month", "day", NA_character_))) {
    warning("unit must be 'year', 'month', 'day', or NA (case-insensitive)")
  }
  mapply(age_to_years_scalar, value, unit)
}


# write nicely-formatted Excel file using openxlsx
write_pretty_xlsx <- function(x, file, fill = "#ffcccb", date_format = "yyyy-mm-dd",
                              firstActiveRow = 2, firstActiveCol = NULL, zoom = 130,
                              group_shade = NULL, overwrite = TRUE) {
  library(openxlsx)
  options("openxlsx.dateFormat" = date_format)
  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "Sheet 1", zoom = zoom)
  
  if (!is.null(group_shade)) {
    x <- x[order(x[[group_shade]]),]
    x[["i"]] <- as.integer(integer_id(x[[group_shade]]) %% 2L)
    x <- x[,c("i", names(x)[names(x) != "i"])]
  }
  
  nrow_x <- nrow(x) + 1
  ncol_x <- ncol(x)
  openxlsx::writeData(wb, 1, x)
  openxlsx::setColWidths(wb, 1, cols = 1:ncol_x, widths = "auto")
  openxlsx::freezePane(wb, 1, firstActiveRow = 2)
  
  if (!is.null(group_shade)) {
    openxlsx::conditionalFormatting(wb, 1,
                                    cols = 1:ncol_x,
                                    rows = 2:nrow_x,
                                    rule = paste0("$A2>0"),
                                    style = openxlsx::createStyle(bgFill = fill))
  }
  
  suppressMessages(openxlsx::saveWorkbook(wb, file = file, overwrite = overwrite))
}


