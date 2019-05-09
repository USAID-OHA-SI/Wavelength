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
#' @param type is the dataset a weekly or monthly file, "weekly", "monthly"
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

structure_tza <- function(filepath, type = "weekly", folderpath_output = NULL){

  if(type == "weekly") {

    #identify variables to keep
      sel_ind <- dplyr::pull(ind_map_tza, ind_label)

    #unwanted tabs (don't have site data)
      remove <- c("Guide-Facility", "Guide-Comm", "Facility Data", "Community Data", "flat file")

    #import all site sheets
      df <- filepath %>%
        readxl::excel_sheets() %>%
        setdiff(remove) %>%
        purrr::set_names() %>%
        purrr::map_dfr(~readxl::read_excel(filepath, sheet = .x, skip = 3, col_types = "text"))

    #filter to select indicators
      df <- df %>%
        dplyr::select(`Start date`:`Site ID (from DATIM)`, dplyr::one_of(sel_ind), -`End date`)

    #munge and reshape long
      df <- df %>%
        # dplyr::select(-dplyr::matches("\\...")) %>% #get rid of extra blank columns at end
        dplyr::select(`Start date`:`Site ID (from DATIM)`, dplyr::one_of(sel_ind)) %>%
        dplyr::mutate_at(dplyr::vars(`Start date`),
                         ~ as.double(.) %>% as.Date(origin = "1899-12-30")) #convert date
      meta <- dplyr::select(df, `Start date`:`Site ID (from DATIM)`) %>% names()
      df <- reshape_long(df, meta)

    #clean names
      df <- df %>%
        dplyr::rename(date = `Start date`,
                      site = `Site Name`,
                      orgunituid = `Site ID (from DATIM)`,
                      fundingagency = Agency) %>%
        dplyr::rename_all(tolower) %>%
        dplyr::mutate(site = stringr::str_remove_all(site, " -.*$| \\."))

    #map indicator name on & filter to just OHA select values
      df <- df %>%
        dplyr::left_join(ind_map_tza, by =  c("ind" = "ind_label")) %>%
        dplyr::select(-ind, -ind.y) %>%
        dplyr::select(-val, dplyr::everything())

    #add reporting pd
      df <- tibble::add_column(df, reporting_freq = "Weekly",
                               .before = "indicator")

  } else if(type == "monthly"){

  #import
    df <- readxl::read_excel(filepath, sheet = "Exported Data")

  #check structure
    #TODO add assert check to make sure structure stays the same

  #identify variables to keep
    sel_ind <- dplyr::pull(ind_map_tza_old, ind)

  #filter to select indicators
    df <- df %>%
      dplyr::select(Partner:Month, dplyr::one_of(sel_ind)) %>%
      dplyr::rename_all(~ tolower(.)) %>%
      dplyr::rename(orgunituid = `datim id`)

  #reshape long and convert val to numeric
    meta <- dplyr::select(df, partner:month) %>% names()
    df <- reshape_long(df, meta)

   #add reporting frequency
    df <- tibble::add_column(df, reporting_freq = "Monthly",
                             .before = "ind")

   #map to standardized indicators
    df <- df %>%
      dplyr::left_join(ind_map_tza_old, by = "ind") %>%
      dplyr::select(-ind) %>%
      dplyr::select(-val, dplyr::everything())

  #convert month into date
    df <- df %>%
      dplyr::mutate(date = stringr::str_replace(month, " ", " 1,") %>% lubridate::mdy()) %>%
      dplyr::select(date, dplyr::everything()) %>%
      dplyr::select(-month)

  #rename agency to match others
    df <- dplyr::rename(df, fundingagency = agency)

  }

  #geo hierarchy alignment
    df <- df %>%
      dplyr::rename(snu1 = region,
                    psnu = council,
                    community = ward,
                    facility = site) %>%
      dplyr::select(-district)

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
    export_hfd(df, folderpath_output)

    return(df)

}
