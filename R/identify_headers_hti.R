#' Create un-umerged, unique header row
#'
#' @param filepath full filepath to the country file
#' @param sheetname tab name
#'
#' @export
#'
#' @examples
#' \dontrun{
#' #get new header row
#'   path <- "~/WeeklyData/Haiti_data.xlsx"
#'   headers <- identify_headers_hti(path, "ALL")
#' #apply new headers to the import
#'  df_hti <- readxl::read_excel(path, sheet = "ALL", skip = 4, col_names = headers) }

identify_headers_hti <- function(filepath, sheetname){

  #first row of headers
    r1 <- extract_headers(filepath, sheetname, 3)

  #second row of headers
    r2 <- extract_headers(filepath, sheetname, 4)

  #align vector lengths to combine
    n <- max(length(r1), length(r2))
    length(r1) <- n
    length(r2) <- n

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



