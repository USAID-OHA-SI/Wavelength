#' Import and Structure Cote d'Ivoire
#'
#' @description This function will be used to take the Cote d'Ivoire HFR
#' dataset and structure in a uniform way to be tidy and align with other
#' OU datasets. Only selected indicators will be imported and transformed
#' based on OHA's needs. The data is sourced from Cote d'Ivoire's partner weekly
#' reporting template files.
#'
#' @param filepath filepath to the output file
#' @param folderpath_output if a txt output is desired, provide the full path to the folder
#'
#' @export
#'
#' @examples
#' \dontrun{
#' #structure output for Cote d'Ivoire
#'   path <- "~/WeeklyData/Raw/CIV_monthly_data.xlsx"
#'   structure_civ(path)
#' #structure output for Cote d'Ivoire & export to txt file
#'   structure_civ(path, "~/WeeklyData/Output") }

structure_civ <- function(filepath, folderpath_output = NULL){

#import
  df <- readxl::read_excel(filepath, col_types = "text")

#filter to relevant variables
  df <- dplyr::filter(df, !indicator %in% c("TX_TO_FIND", "TX_FOUND"))

#adjust date
  df <- dplyr::mutate(df, date = lubridate::as_date(as.integer(date), origin = "1899-12-30"))

#add disagg name
  df <- dplyr::mutate(df, disaggregate = "Age/Sex")

#standardize variable order
  df <- order_vars(df)

#export
  export_hfr(df, folderpath_output)

  invisible(df)
}
