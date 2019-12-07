#' Generate Gap Targets
#'
#' @param df data frame created by `extract_datim()`
#' @param quarters_complete MER quarters with data available
#'
#' @export
#'
#' @examples
#' \dontrun{
#'  myuser <- "UserX"
#'  mech_x_targets <- extract_datim(00001, myuser, mypwd(myuser))
#'  mech_x_targets <- hfr_gap_target(mech_x_targets, 2) }

hfr_gap_target <- function(df, quarters_complete){

  weeks_remaining <- 52 - (quarters_complete * 13)

  if(!"mer_results" %in% names(df))
    df <- dplyr::mutate(df, mer_results = NA)
  if(!"mer_targets" %in% names(df))
    df <- dplyr::mutate(df, mer_targets = NA)

  df <- df %>%
    dplyr::mutate(weekly_targets = mer_targets / 52,
                  targets_gap = ifelse((mer_targets - mer_results) < 0,0, mer_targets - mer_results),
                  weekly_targets_gap = targets_gap/weeks_remaining)
  return(df)
}
