#' Convert dates to date format
#'
#' @param df HFR data frame imported via `import_hfr()`
#' @param round_date rounds date to the nearest HFRweek start (for non-compliance), default = FALSE
#'
#' @export

fix_date <- function(df, round_date = FALSE){

  #adjust Excel formated dates
    df_date_excel <- df %>%
      dplyr::filter(stringr::str_detect(date, "^[:digit:]{5}")) %>%
      dplyr::mutate(date = as.Date(as.numeric(date), origin = "1899-12-30"))

  #adjust ISO formated dates
    df_date_iso <- df %>%
      dplyr::filter(stringr::str_detect(date,  "^[:digit:]{4}-")) %>%
      dplyr::mutate(date = as.Date(date))

  #adjust character date, assuming format is mdy
    df_date_mdy <- df %>%
      dplyr::filter(stringr::str_detect(date,  "^[:digit:]{2}(-|/)")) %>%
      dplyr::mutate(date = lubridate::mdy(date))

  #replace other dates as NA
    df_date_other <- df %>%
      dplyr::filter(stringr::str_detect(date, "^[:digit:]{4}-|^[:digit:]{5}|^[:digit:]{2}(-|/)", negate = TRUE)) %>%
      dplyr::mutate(date = as.Date(NA))

  #bind all types together
    df_date_fixed <- dplyr::bind_rows(df_date_excel, df_date_iso, df_date_mdy, df_date_other)

  #round date (fixes non-compliance)
   if(round_date == TRUE)
     df_date_fixed <- round_date(df_date_fixed)

  return(df_date_fixed)
}

#' Round to nearest HFR date
#'
#' @param df df HFR data frame imported via `import_hfr()`
#'
#' @export

round_date <- function(df){
  if(var_exists(df, date)){
    df <-
      dplyr::mutate(date = dplyr::case_when(is.na(date) ~ as.Date(NA),
                                            lubridate::wday(date) == 1 ~
                                              lubridate::ceiling_date(date, unit = "week",
                                                                      week_start = 1),
                                            lubridate::wday(date)  > 1 ~
                                              lubridate::floor_date(date, unit = "week",
                                                                    week_start = 1)))
  }

  return(df)
}
