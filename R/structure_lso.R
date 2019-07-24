#' Import and Structure Lesotho
#'
#' @description This function will be used to take the weekly Lesotho
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
#' #structure output for Lesotho
#'   path <- "~/WeeklyData/Raw/LSO_Weekly.xlsx"
#'   structure_lso(path)
#' #structure output for Lesotho & export to txt file
#'   structure_lso(path, "~/WeeklyData/Output") }

structure_lso <- function(filepath, folderpath_output = NULL){

  #import
    df <- readxl::read_excel(filepath)

  #identify variables to keep
    meta <- c("FACILITY", "DISTRICT", "CWEEKDATE")
    sel_ind <- ind_map_lso %>%
      dplyr::filter(!is.na(indicator)) %>%
      dplyr::pull(ind)

  #filter to select indicators
    df <- df %>%
      dplyr::select(dplyr::one_of(meta), dplyr::one_of(sel_ind))

  #reshape long
    df <- reshape_long(df, meta)
      rm(meta, sel_ind)

  #clean up date
    df <- df %>%
      tidyr::separate(CWEEKDATE, c(NA, "date"), sep = "- ") %>%
      dplyr::mutate(date = lubridate::dmy(date))

  #clean up headers
    df <- df %>%
      dplyr::rename(facility = FACILITY,
                    psnu = DISTRICT)

  #map to standardized indicators
    df <- df %>%
      dplyr::left_join(ind_map_lso, by = "ind") %>%
      dplyr::select(-ind) %>%
      dplyr::select(-val, dplyr::everything())

  #add reporting frequency
    df <- tibble::add_column(df, reporting_freq = "Weekly",
                             .before = "indicator")

  #add FY
    df <- df %>%
      dplyr::select(date, dplyr::everything()) %>%
      assign_pds()

  #add operatingunit
    df <- add_ou(df, "Lesotho")

  #add funding agency
    df <- tibble::add_column(df, fundingagency = "USAID",
                             .after = "operatingunit")

  #standardize variable order
    df <- order_vars(df)

  #export
    export_hfr(df, folderpath_output)

    return(df)

}

