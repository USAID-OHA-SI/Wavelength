#' Create un-umerged, unique header row
#'
#' @param filepath full filepath to the country file
#'
#' @export
#'
#' @examples
#' \dontrun{
#' #get new header row
#'   path <- "~/WeeklyData/Burundi_data.xlsx"
#'   headers <- identify_headers_bdi(path)
#' #apply new headers to the import
#'  df_bdi <- readxl::read_excel(path, col_names = headers) }

identify_headers_bdi <- function(filepath){

  #first row of headers
  r1 <- extract_headers(filepath, skip = 0)

  #second row of headers
  r2 <- extract_headers(filepath, skip = 1)

  #combine, replacing "" with NA & filling down (merged headers) before concatenating
  headers <-
    tibble::tibble(r1, r2) %>%
    dplyr::mutate_all(~ifelse(stringr::str_length(.) == 0, NA, .)) %>%
    dplyr::mutate(r2 = ifelse(is.na(r1) & is.na(r2), "drop", r2)) %>%
    tidyr::fill(r1) %>%
    tidyr::unite(header, sep = ".") %>%
    dplyr::pull()

  return(headers)
}
