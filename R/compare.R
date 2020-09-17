#' Compare two most-recent compilation files for given OC and produce summary
#' of patient_id that were present in the first compilation but not the next
#'
#' @param OC 
#' @param path_export 
#'
#' @examples
compare_ids <- function(OC, path_export) {
  
  ## get paths to OC-specific compilation files
  files <- sort(
    llutils::list_files(
      file.path(path_export, OC),
      pattern = "msf_covid19_linelist.*\\.xlsx",
      full.names = TRUE
    )
  )
  
  ## subset to 2 most-recent compilations
  files <- tail(files, 2)
  compilation_dates <- llutils::extract_date(files)
  
  ## read 2 most recent compilations
  compilations <- purrr::map(
    setNames(files, compilation_dates),
    readxl::read_xlsx,
    col_types = "text"
  )
  
  ## relevant indices
  i_latest <- length(compilations)
  ii <- seq_len(length(compilations) - 1L)

  ## prepare list for output
  out <- list()
  
  ## compare
  for (i in ii) {
    out[[i]] <- compare_ids_(
      d = compilations[[i]],
      d_next= compilations[[i+1]],
      d_latest = compilations[[i_latest]],
      date = compilation_dates[i],
      date_next = compilation_dates[i+1],
      date_latest = compilation_dates[i_latest]
    )
    
  }
  
  dplyr::bind_rows(out)
}


compare_ids_ <- function(d,
                         d_next,
                         d_latest,
                         date,
                         date_next,
                         date_latest) {
  
  d_next_new <- d_next %>% 
    mutate(date_next = date_next) %>% 
    select(
      site,
      linelist_row,
      next__compilation_date = date_next,
      next_at_row__upload_date = upload_date,
      next_at_row__MSF_N_Patient = MSF_N_Patient,
      next_at_row__patient_id = patient_id
    )
  
  d_latest_new <- d_latest %>% 
    mutate(date_latest = date_latest) %>% 
    select(
      site,
      linelist_row,
      latest__compilation_date = date_latest,
      latest_at_row__upload_date = upload_date,
      latest_at_row__MSF_N_Patient = MSF_N_Patient,
      latest_at_row__patient_id = patient_id
    )
  
  d %>% 
    mutate(compilation_date = date) %>% 
    select(compilation_date, upload_date, site, linelist_row, MSF_N_Patient, patient_id) %>% 
    anti_join(d_next, by = "patient_id") %>% 
    left_join(d_next_new, by = c("site", "linelist_row")) %>% 
    left_join(d_latest_new, by = c("site", "linelist_row")) %>% 
    mutate(
      next__compilation_date = date_next,
      latest__compilation_date = date_latest
    )
}

