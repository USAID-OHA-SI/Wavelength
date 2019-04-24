#' Create un-umerged, unique header row for Cameroon
#'
#' @param filepath full filepath to the country file
#' @param sheetname tab name
#'
#' @export
#'
#' @examples
#' \dontrun{
#' #get new header row
#'   path <- "~/WeeklyData/Cameroon_data.xlsx"
#'   headers <- identify_headers_cameroon(path, "TX_NEW")
#' #apply new headers to the import
#'  df_hti <- readxl::read_excel(path, sheet = "TX_NEW", skip = 1, col_names = headers) }

identify_headers_cmr <- function(filepath, sheetname){

  #first row of headers
  r1 <- extact_headers(filepath, sheetname, 1)

  #second row of headers
  r2 <-  extact_headers(filepath, sheetname, 2)

  #third row of headers
  r3 <-  extact_headers(filepath, sheetname, 3)

  #align vector lengths to combine
  n <- max(length(r1), length(r2), length(r3))
  length(r1) <- n
  length(r2) <- n
  length(r3) <- n

  #combine, replacing "" with NA & filling down (merged headers) before concatenating
  headers <-
    tibble::tibble(r1, r2, r3) %>%
    dplyr::mutate_all(~ifelse(stringr::str_length(.) == 0, NA, .)) %>%
    dplyr::mutate(r2 = ifelse(is.na(r1) & is.na(r2) & is.na(r3), "drop", r2)) %>%
    tidyr::fill(r1, r2) %>%
    tidyr::unite(header, sep = ".") %>%
    dplyr::mutate(header = ifelse(duplicated(.), paste(header, dplyr::row_number(), sep = "_"), header)) %>%
    dplyr::pull()

  return(headers)
}
