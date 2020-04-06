#' Validation on import
#'
#' @param df df create during `hfr_import()`
#'
#' @export

validate_import <- function(df){

  #check columns
  if(var_exists(df, "val")){
    req_cols <- template_cols_long
  } else if(var_exists(df, "hts_tst.NA.NA")){
    req_cols <- template_cols_wide_lim
  } else {
    req_cols <- template_cols_wide
  }

  missing <- flag_missing(req_cols, names(df))
  extra <- flag_extra(req_cols, names(df))

  #PRINT VALIDATION

  cat("\nAre there any missing columns on import?", missing,
      "\nAre there any extra columns on import?", extra)

}
