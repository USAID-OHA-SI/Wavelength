#' Import and Munge HFR Standard Templates
#'
#' @param filepath filepath to sumbitted template
#' @param folderpath_output if a txt output is desired, provide the full path to the folder
#'
#' @export


process_template <- function(filepath, folderpath_output = NULL){

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
      fix_date() %>%
      assign_pds()

  #convert to numeric
    df <- dplyr::mutate(df, val = as.double(val))

  #aggregate to combine rows where needed (minimize row count)
    df <- df %>%
      dplyr::group_by_at(dplyr::vars(setdiff(names(df), "val"))) %>%
      dplyr::summarise_at(dplyr::vars(val), sum, na.rm = TRUE) %>%
      dplyr::ungroup()

  #export
    export_hfr(df, folderpath_output)

  invisible(df)
}







