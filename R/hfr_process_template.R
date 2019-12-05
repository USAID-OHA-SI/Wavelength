#' Import and Munge HFR Standard Templates
#'
#' @param filepath filepath to sumbitted template
#' @param folderpath_output if a txt output is desired, provide the full path to the folder
#' @param round_date rounds date to the nearest HFRweek start (for non-compliance), default = FALSE
#'
#' @export


hfr_process_template <- function(filepath, round_hfrdate = FALSE, folderpath_output = NULL){

  #import template sheet(s)
    df <- hfr_import(filepath)

  #reshape wide to match long df (only affects wide format)
    df <- hfr_gather(df)

  #clean up string variables
    df <- hfr_munge_string(df)

  #resolve issues with non-standard data entry
    df <- hfr_fix_noncompliance(df)

  #adjust date & assign fy + pd
    df <- df %>%
      hfr_fix_date(round_hfrdate) %>%
      hfr_assign_pds()

  #aggregate to combine rows where needed (minimize row count)
    df <- hfr_aggr(df)

  #export
    hfr_export(df, folderpath_output)

  invisible(df)
}







