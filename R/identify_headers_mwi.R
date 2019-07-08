#' Create un-umerged, unique header row
#'
#' @param filepath full filepath to the country file
#'
#' @export
#'
#' @examples
#' \dontrun{
#' #get new header row
#'   path <- "~/WeeklyData/Malawi_data.xlsx"
#'   headers <- identify_headers_mwi(path)
#' #apply new headers to the import
#'  df_mwi <- readxl::read_excel(path, col_names = headers) }

identify_headers_mwi <- function(filepath){

  #first row of headers
  r1 <- extract_headers(filepath, "data by week April& May", skip = 0)

  #second row of headers
  r2 <- extract_headers(filepath,  "data by week April& May", skip = 1)

  #combine, replacing "" with NA & filling down (merged headers) before concatenating
  headers <-
    tibble::tibble(r1, r2) %>%
    dplyr::mutate_all(~ifelse(stringr::str_length(.) == 0, NA, .)) %>%
    tidyr::fill(r1) %>%
    tidyr::unite(header, sep = ".") %>%
    dplyr::mutate(header = stringr::str_remove(header, "\\.NA$")) %>%
    dplyr::pull()

  return(headers)
}
