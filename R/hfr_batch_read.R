#' Batch read hfr files
#'
#' @param pfolder processed file folder
#' @param pattern filename pattern
#' @param source append source filename?
#' @return df
#' @export
#' @example
#' \dontrun{
#'   hfr_batch_read('./Data/2020.05', pattern='HFR_FY2020.05')
#' }
#'
hfr_batch_read <- function(pfolder, pattern, source=FALSE) {

  files <- list.files(pfolder, pattern = pattern, full.names = TRUE)

  if (length(files) == 0) {
    stop("Folder seems to be empty or pattern did not match any of the files ...")
  }

  hfr_data <- files %>%
    purrr::map_dfr(Wavelength::hfr_read, .id = "id") %>%
    dplyr::mutate(id = as.integer(id))

  if (source == TRUE) {

    hfr_files <- list.files(pfolder, pattern = pattern, full.names = FALSE) %>%
      as.data.frame(stringsAsFactors=FALSE) %>%
      dplyr::mutate(id = dplyr::row_number()) %>%
      purrr::set_names(c("source", 'id'))

    hfr_data <- hfr_data %>%
      dplyr::left_join(hfr_files, by="id")
  }

  hfr_data <- hfr_data %>%
    dplyr::select(-id)

  return(hfr_data)
}
