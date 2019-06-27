#' Import and Structure Kenya
#'
#' @description This function will be used to take the Kenya HFR
#' dataset and structure in a uniform way to be tidy and align with other
#' OU datasets. Only selected indicators will be imported and transformed
#' based on OHA's needs.
#'
#' @param filepath filepath to the output file
#' @param folderpath_output if a txt output is desired, provide the full path to the folder
#'
#' @export
#'
#' @examples
#' \dontrun{
#' #structure output for Kenya
#'   path <- "~/WeeklyData/Raw/KEN_Weekly.xlsx"
#'   structure_ken(path)
#' #structure output for Kenya & export to txt file
#'   structure_ken(path, "~/WeeklyData/Output") }

structure_ken <- function(filepath, folderpath_output = NULL){

#Afya Pwani HFR 24.06.2019.xlsx"

#import
  df <- readxl::read_excel(path, col_types = "text")

#clean up column names
  df <- df %>%
    dplyr::rename_all(tolower) %>%
    dplyr::rename(operatingunit = orgunitlevel1,
                  snu1 = county,
                  psnu = ward,
                  val = value) %>%
    dplyr::select(-c(`facility mfl code`, `sub-county`))

#clean up mechanism ID, FY and value
  df <- df %>%
    dplyr::mutate_at(dplyr::vars(mechanismid, fy), ~ as.integer(.) %>% as.character()) %>%
    dplyr::mutate(val = as.numeric(val))

#remove "yrs" from agecoarse
  df <- dplyr::mutate(df, agecoarse = stringr::str_remove(agecoarse, "yrs"))

#fix excel date
  df <- dplyr::mutate(df, date = lubridate::as_date(as.integer(date), origin = "1899-12-30"))

#add disaggregate
  df <- dplyr::mutate(df, disaggregate = "Age/Sex")

#standardize variable order
  df <- order_vars(df)

#export
  export_hfr(df, folderpath_output)
}
