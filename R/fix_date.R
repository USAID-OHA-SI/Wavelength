
#' Convert dates to date format
#'
#' @param df HFR data frame imported via `import_hfr()`
#'
#' @export

fix_date <- function(df){

  if(is_excel_date(df)){
    df <- dplyr::mutate(df, date = as.Date(as.numeric(date), origin = "1899-12-30"))
  } else if(is_iso_date(df)){
    df <- dplyr::mutate(df, date = as.Date(date))
  } else {
    df <- dplyr::mutate(df, date = as.Date(NA))
  }

  return(df)
}



#' Determine if dates are in Excel format
#'
#' @param df HFR data frame imported via `import_hfr()`
#'
#' @export

is_excel_date <- function(df){
  df %>%
    dplyr::distinct(date) %>%
    dplyr::pull() %>%
    stringr::str_detect("[:digit:]{5}") %>%
    all() == TRUE
}

#' Determine if dates are in Excel format
#'
#' @param df  HFR data frame imported via `import_hfr()`
#'
#' @export

is_iso_date <- function(df){
  df %>%
    dplyr::distinct(date) %>%
    dplyr::pull() %>%
    stringr::str_detect("[:digit:]{4}-") %>%
    all() == TRUE
}
