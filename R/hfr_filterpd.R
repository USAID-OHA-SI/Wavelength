#' Filter to Select HFR Period
#'
#' @param df HFR data frame imported via `hfr_import()`
#' @param hfr_pd_sel HFR reporting period, 1-13, no filter when NULL, default = NULL
#' @param hfr_fy_sel fiscal year, default = 2020
#'
#' @export

hfr_filter_pd <- function(df, hfr_pd_sel = NULL, hfr_fy_sel = 2020){


  if(!is.numeric(hfr_fy_sel))
    stop("Enter hfr_fy_sel as an integer, eg 2020")

  if(!is.null(hfr_pd_sel) && !is.numeric(hfr_pd_sel))
    stop("Enter hfr_pd_sel as an integer between 1 and 13")

  if(!is.null(hfr_pd_sel))
    df <- df %>%
      dplyr::filter(fy == hfr_fy_sel,
                    hfr_pd == hfr_pd_sel)
  return(df)
}
