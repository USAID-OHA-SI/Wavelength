#' Import and Structure Tanzania
#'
#' @description This function will be used to take the monthly Tanzania
#' dataset and structure in a uniform way to be tidy and align with other
#' OU datasets. Only selected indicators will be imported and transformed
#' based on OHA's needs. The data is sourced from Tazania's monthly reporting
#' platform \url{http://hmis.reachproject.or.tz/MonthlyPortal/}
#'
#' @param filepath filepath to the output file
#' @param folderpath_output if a txt output is desired, provide the full path to the folder
#'
#' @export
#'
#' @examples
#' \dontrun{
#' #structure output for Tanzania
#'   path <- "~/WeeklyData/Raw/All Monthly Raw Data in given timeframe.xlsx"
#'   structure_tza(path)
#' #structure output for Tanzania & export to txt file
#'   structure_tza(path, "~/WeeklyData/Output") }

structure_tza <- function(filepath, folderpath_output = NULL){

  #import
    df <- readxl::read_excel(filepath, sheet = "Exported Data")

  #check structure
    #TODO add assert check to make sure structure stays the same

  #identify variables to keep
    sel_ind <- dplyr::pull(ind_map_tza, ind)

  #filter to select indicators, reshape long, and make all vars lower
    df <- df %>%
      dplyr::select(Partner:Month, dplyr::one_of(sel_ind)) %>%
      tidyr::gather(ind, result, -Partner:-Month, na.rm = TRUE) %>%
      dplyr::filter(result != 0) %>%
      dplyr::rename_all(~ tolower(.))

  #geo hierarchy alignment
    df <- df %>%
      dplyr::rename(orgunituid = `datim id`,
                    snu1 = region,
                    psnu = council,
                    community = ward,
                    facility = site) %>%
      dplyr::select(-district)

   #change var names -> date and agency
    df <- df %>%
      dplyr::rename(dateasentered = month,
                    fundingagency = agency)

   #add operatingunit
    df <- df %>%
      dplyr::mutate(operatingunit = "Tanzania") %>%
      dplyr::select(operatingunit, dplyr::everything())

   #add reporting frequency
    df <- tibble::add_column(df, reporting_freq = "Monthly",
                             .before = "dateasentered")

   #map to standardized indicators
    df <- df %>%
      dplyr::left_join(ind_map_tza, by = "ind") %>%
      dplyr::select(-result, dplyr::everything())

   #adjust Index testing (comm where there is no site)
    df <- dplyr::mutate(df, indicator =
                        dplyr::case_when(indicator == "HTS_INDEX" & is.na(facility) ~ stringr::str_replace(indicator, "INDEX", "INDEX_COM"),
                                         indicator == "HTS_INDEX"  ~ stringr::str_replace(indicator, "INDEX", "INDEX_FAC"),
                                         TRUE ~ indicator))
  #export
    if(!is.null(folderpath_output))
      readr::write_tsv(df, file.path(folderpath_output, paste0("TZA_HFD_", format(Sys.Date(),"%Y%m%d"), ".txt")), na = "")

    return(df)

}
