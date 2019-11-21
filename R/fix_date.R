#' Convert dates to date format
#'
#' @param df HFR data frame imported via `import_hfr()`
#'
#' @export

fix_date <- function(df){

  #adjust Excel formated dates
    df_date_excel <- df %>%
      dplyr::filter(stringr::str_detect(date, "[:digit:]{5}")) %>%
      dplyr::mutate(date = as.Date(as.numeric(date), origin = "1899-12-30"))

  #adjust ISO formated dates
    df_date_iso <- df %>%
      dplyr::filter(stringr::str_detect(date,  "[:digit:]{4}-")) %>%
      dplyr::mutate(date = as.Date(date))

  #adjust character date, assuming format is mdy
    df_date_mdy <- df %>%
      dplyr::filter(stringr::str_detect(date,  "[:digit:]{2}(-|/)")) %>%
      dplyr::mutate(date = lubridate::mdy(date))

  #replace other dates as NA
    df_date_other <- df %>%
      dplyr::filter(stringr::str_detect(date, "[:digit:]{4}-|[:digit:]{5}|[:digit:]{2}(-|/)", negate = TRUE)) %>%
      dplyr::mutate(date = as.Date(NA))

  #bind all types together
    df_date_fixed <- dplyr::bind_rows(df_date_excel, df_date_iso, df_date_mdy, df_date_other)

  return(df_date_fixed)
}
