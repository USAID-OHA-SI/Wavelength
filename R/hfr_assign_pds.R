#' Create a data frame of HFR weeks and periods
#'
#' @param fy fiscal year
#' @param week are periods weekly? default = FALSE
#' @export

hfr_identify_pds <- function(fy = NULL, week = FALSE){

  if(is.null(fy)){
    fy <- curr_fy
    print(paste("No fy provided; using", curr_fy))
  }

  fy_start <- paste0(fy-1, "-10-01") %>%
    as.Date()

  if(fy == 2020){
    fy_start <- lubridate::floor_date(fy_start, unit = "week", week_start = 1)

    date <- seq(fy_start, by = 7, length.out = 52)

    hfr_pd <- rep(1:13, each = 4)

  } else if (week == TRUE) {
    fy_start <- lubridate::ceiling_date(fy_start, unit = "week", week_start = 1)
    date <- seq(fy_start, by = 7, length.out = 52)

    hfr_pd <- date %>%
      tibble::tibble() %>%
      dplyr::mutate(date_adj = lubridate::floor_date(date, unit = "month") - months(9),
                    hfr_pd = lubridate::month(date_adj)) %>%
      dplyr::pull(hfr_pd)

  } else {
    date <- seq(fy_start, by = "month", length.out = 12)

    hfr_pd <- seq(1:12)
  }

  df_dates <- dplyr::bind_cols(date = date, hfr_pd = hfr_pd) %>%
    dplyr::mutate(fy = lubridate::quarter(date, with_year = TRUE, fiscal_start = 10) %>%
                    stringr::str_sub(., 1, 4) %>% as.numeric,
                  fy = ifelse(hfr_pd == 1 & lubridate::month(date) == 9, fy + 1, fy)) %>%
    dplyr::select(date, fy, hfr_pd)

  return(df_dates)
}



#' Add HFR Period column
#'
#' @param df HFR data frame with date
#'
#' @export
#'
#' @examples
#' \dontrun{
#'  df <- hfr_assign_pds(df) }

hfr_assign_pds <- function(df){

  if(!var_exists(df, "date"))
    stop("`date` column does not exist in the supplied data frame")

  fy_start <- min(df$date, na.rm = TRUE) %>%
    lubridate::quarter(with_year = TRUE, fiscal_start = 10) %>%
    stringr::str_sub(1, 4) %>%
    as.numeric()

  if(fy_start == 2020){

    fy_end   <- (max(df$date, na.rm = TRUE) + 7) %>%
      lubridate::quarter(with_year = TRUE, fiscal_start = 10) %>%
      stringr::str_sub(1, 4) %>%
      as.numeric()

    pds <- purrr::map_dfr(c(fy_start:fy_end), hfr_identify_pds) %>%
      dplyr::rename_at(dplyr::vars(fy, hfr_pd), ~ paste0(., "_drop"))

    df <- df %>%
      dplyr::select(-dplyr::matches("fy|hfr_pd")) %>%
      tibble::add_column(fy = NA, hfr_pd = NA, .after = "date")

    df <- df %>%
      dplyr::left_join(pds, by = "date") %>%
      dplyr::mutate(fy = fy_drop,
                    hfr_pd = hfr_pd_drop) %>%
      dplyr::select(-dplyr::ends_with("drop"))

  } else {

    df <- df %>%
      dplyr::mutate(hfr_pd = lubridate::month((date - months(9))),
                    fy = lubridate::quarter(date, with_year = TRUE, fiscal_start = 10) %>%
                      stringr::str_sub(., 1, 4) %>% as.numeric,
                    fy = ifelse(hfr_pd == 1 & lubridate::month(date) == 9, fy + 1, fy)) %>%
      dplyr::select(date, fy, hfr_pd, hfr_freq, dplyr::everything())
  }


  return(df)
}

