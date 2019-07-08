#' Import and Structure Tanzania
#'
#' @description This function will be used to take the monthly Tanzania
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
#' #structure output for Tanzania
#'   path <- "~/WeeklyData/Raw/All Monthly Raw Data in given timeframe.xlsx"
#'   structure_tza(path)
#' #structure output for Tanzania & export to txt file
#'   structure_tza(path, "~/WeeklyData/Output") }

structure_tza <- function(filepath, folderpath_output = NULL){

  #import
    df <- readxl::read_xlsx(filepath, sheet = "Weekly", col_type = "text")

  #filter to select indicators
    sel_ind <- dplyr::pull(ind_map_tza, ind_label)
    df <- df %>%
      dplyr::select(`Start date`:`Site ID (from DATIM)`, dplyr::one_of(sel_ind), -`End date`)

  #fix date (one day off, start on Sundays)
    df <- df %>%
      dplyr::mutate(date = as.Date(as.double(`Start date`), origin = "1899-12-30") + 1) %>%
      dplyr::select(date, dplyr::everything(), -`Start date`)

  #munge and reshape long
    meta <- dplyr::select(df, date:`Site ID (from DATIM)`) %>% names()
    df <- reshape_long(df, meta)

  #clean names
    df <- df %>%
      dplyr::rename(facility = `Site Name`,
                    orgunituid = `Site ID (from DATIM)`,
                    snu1 = Region,
                    psnu = Council,
                    community = Ward,
                    fundingagency = Agency) %>%
      dplyr::select(-District) %>%
      dplyr::rename_all(tolower) %>%
      dplyr::mutate(facility = stringr::str_remove_all(facility, " -.*$| \\."))

  #map indicator name on & filter to just OHA select values
    df <- df %>%
      dplyr::left_join(ind_map_tza, by =  c("ind" = "ind_label")) %>%
      dplyr::select(-ind, -ind.y) %>%
      dplyr::select(-val, dplyr::everything())

  #add reporting pd
    df <- tibble::add_column(df, reporting_freq = "Weekly",
                             .before = "indicator") %>%
      dplyr::mutate(reporting_freq = ifelse(indicator == "TX_CURR", "Monthly", reporting_freq))

  #add FY
    df <- df %>%
      dplyr::mutate(fy = lubridate::quarter(date, with_year = TRUE, fiscal_start = 10) %>%
                      stringr::str_sub(., 1, 4)) %>%
      dplyr::select(date, fy, dplyr::everything())

  #add operatingunit
    df <- add_ou(df, "Tanzania")

  #standardize variable order
    df <- order_vars(df)

  #export
    export_hfr(df, folderpath_output)

  invisible(df)

}
