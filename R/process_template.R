#' Import and Munge HFR Standard Templates
#'
#' @param filepath filepath to sumbitted template
#' @param folderpath_output if a txt output is desired, provide the full path to the folder
#' @param round_date rounds date to the nearest HFRweek start (for non-compliance), default = FALSE
#'
#' @export


process_template <- function(filepath, round_hfrdate = FALSE, folderpath_output = NULL){

  #import template sheet(s)
    df <- import_hfr(filepath)

  #reshape wide to match long df (only affects wide format)
    df <- gather_hfr(df)

  #clean up string variables
    df <- munge_string(df)

  #resolve issues with non-standard data entry
    df <- fix_noncompliance(df)

  #adjust date & assign fy + pd
    df <- df %>%
      fix_date(round_hfrdate) %>%
      assign_pds()

  #aggregate to combine rows where needed (minimize row count)
    df <- aggr_hfr(df)

  #export
    export_hfr(df, folderpath_output)

  invisible(df)
}







