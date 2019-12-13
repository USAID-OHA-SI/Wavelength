#' Restrict HFR data frame columns
#'
#' @param df HFR data frame imported via `hfr_import()`
#'
#' @export

hfr_restrict_cols <- function(df){

  if(var_exists(df, "val")){
    cols <- intersect(template_cols_long, names(df))
    df <- dplyr::select_at(df, cols)
  } else {
    cols <- intersect(template_cols_wide, names(df))
    df <- dplyr::select_at(df, cols)
  }

  return(df)
}
