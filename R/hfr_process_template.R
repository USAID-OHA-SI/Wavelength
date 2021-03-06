#' Import and Munge HFR Standard Templates
#'
#' @param filepath filepath to sumbitted template
#' @param round_hfrdate rounds date to the nearest HFRweek start (for non-compliance), default = FALSE
#' @param hfr_pd_sel filter for HFR reporting period, 1-13, no filter when NULL, default = NULL
#' @param folderpath_output if a csv output is desired, provide the full path to the folder
#' @param datim_path path to datim lookup files
#'
#' @export

hfr_process_template <- function(filepath,
                                 round_hfrdate = FALSE,
                                 hfr_pd_sel = NULL,
                                 folderpath_output = NULL,
                                 datim_path = NULL){

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

  #validation on reporting type
    check_frequency(df)

  #clean up string variables
    df <- hfr_munge_string(df)

  #resolve issues with non-standard data entry
    df <- hfr_fix_noncompliance(df)

  #adjust date & assign fy + pd
    df <- df %>%
      hfr_fix_date(round_hfrdate) %>%
      hfr_assign_pds()

  #filter if required to specific pd
    df <- hfr_filter_pd(df, hfr_pd_sel, hfr_fy_sel = curr_fy)

  #aggregate to combine rows where needed (minimize row count)
    df <- hfr_aggr(df)

  #validation checks: full checks if datim_path provided
    if (!is.null(datim_path)) {
      check_cnt <- TRUE
      path_files <- datim_path
    } else {
      check_cnt <- FALSE
      path_files <- NULL
    }

    df <- validate_output(df,
                          output_path=folderpath_output,
                          content=check_cnt,
                          datim_path = path_files)

  #export
    hfr_export(df, folderpath_output, by_mech = TRUE)

  invisible(df)
}







