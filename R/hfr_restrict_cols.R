#' Restrict HFR data frame columns
#'
#' @param df HFR data frame imported via `hfr_import()`
#'
#' @export

hfr_restrict_cols <- function(df){

  if(var_exists(df, "val")){
    cols <- intersect(template_cols_long, names(df))
  } else if(var_exists(df, "hts_tst.NA.NA")){
    cols <- intersect(template_cols_wide_lim, names(df))
  } else {
    cols <- intersect(template_cols_wide, names(df))
  }
    df <- dplyr::select_at(df, cols)

  return(df)
}
