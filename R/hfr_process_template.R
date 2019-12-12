#' Import and Munge HFR Standard Templates
#'
#' @param filepath filepath to sumbitted template
#' @param round_hfrdate rounds date to the nearest HFRweek start (for non-compliance), default = FALSE
#' @param folderpath_output if a csv output is desired, provide the full path to the folder
#'
#' @export


hfr_process_template <- function(filepath, round_hfrdate = FALSE, folderpath_output = NULL){

  #validation checks
    validate_initial(filepath)

  #import template sheet(s)
    df <- hfr_import(filepath)

  #validation checks
    validate_import(df)

  #remove any extra columns
    df <- hfr_restrict_cols(df)

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

  #validation checks
    validate_output(df)

  #export
    hfr_export(df, folderpath_output)

  invisible(df)
}







