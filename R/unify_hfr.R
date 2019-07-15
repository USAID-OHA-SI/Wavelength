#' Unify High Frequency Reporting Dataset
#'
#' @param ou_iso ISO code for the operating unit
#' @param filepath filepath to the output file
#' @param folderpath_output if a txt output is desired, provide the full path to the folder
#'
#' @export
#'
#' @examples
#' \dontrun{
#' #structure output for Tanzania
#'   path <- "~/WeeklyData/Raw/All Monthly Raw Data in given timeframe.xlsx"
#'   unify_hfr("TZA", path, "~/WeeklyData/Output") }

unify_hfr <- function(ou_iso, filepath, folderpath_output = NULL){
  switch(ou_iso,
         BDI = structure_bdi(filepath, folderpath_output),
         CIV = structure_civ(filepath, folderpath_output),
         ETH = structure_eth(filepath, folderpath_output),
         HTI = structure_hti(filepath, folderpath_output),
         KEN = structure_ken(filepath, folderpath_output),
         LSO = structure_lso(filepath, folderpath_output),
         TZA = structure_tza(filepath, folderpath_output),
         UGA = structure_uga(filepath, folderpath_output),
         ZMB = structure_zmb(filepath, folderpath_output),
         ZAF = structure_zaf(filepath, folderpath_output)
         )
}

