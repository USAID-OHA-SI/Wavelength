#' Restrict HFR data frame columns
#'
#' @param df HFR data frame imported via `hfr_import()`
#'
#' @export

hfr_restrict_cols <- function(df){

  if(var_exists(df, "val")){
    df <- dplyr::select_at(df, template_cols_long)
  } else {
    df <- dplyr::select_at(df, template_cols_wide)
  }

  return(df)
}
