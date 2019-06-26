#' Import and Structure Burundi
#'
#' @description This function will be used to take the weekly Burundi
#' dataset and structure in a uniform way to be tidy and align with other
#' OU datasets. Only selected indicators will be imported and transformed
#' based on OHA's needs. The data is sourced from Burundi's partner weekly
#' reporting template files.
#'
#' @param filepath filepath to the output file
#' @param folderpath_output if a txt output is desired, provide the full path to the folder
#'
#' @export
#'
#' @examples
#' \dontrun{
#' #structure output for Burundi
#'   path <- "~/WeeklyData/Raw/BDI_monthly_data.xlsx"
#'   structure_hti(path)
#' #structure output for Burundi & export to txt file
#'   structure_hti(path, "~/WeeklyData/Output") }

structure_bdi <- function(filepath, folderpath_output = NULL){

  #import; headers are across 2 rows, so we need to identify and create them
    headers <- identify_headers_bdi(filepath)
    df <- readxl::read_excel(filepath, skip = 2, col_names = headers, col_types = "text")

  #keep only select variables (no PMTCT & totals for HTS & TX)
    df <- df %>%
      dplyr::select(dplyr::ends_with("NA"), dplyr::contains("Total"), dplyr::starts_with("TX"))

  #reshape long
    df <- df %>%
      tidyr::gather(ind, val, -dplyr::ends_with("NA"))

  #clean up indicators & variable headers
    df <- df %>%
      dplyr::left_join(ind_map_bdi, by = "ind") %>%
      dplyr::select(psnu= Province.NA, community = `Health district.NA`,
                    facility = Facility.NA, indicator, disaggregate,
                    otherdisaggregate, val)

  #add ou
    df <- add_ou(df, "Burundi")

  #add reporting frequency
    df <- tibble::add_column(df, reporting_freq = "Weekly",
                             .after = "facility")

  #identify reporting date from sheet name and add to df
    report_date <- readxl::excel_sheets(filepath) %>%
      stringr::str_remove("-[:digit:]{1,}") %>%
      lubridate::dmy()
    df <- df %>%
      tibble::add_column(date = report_date, fy = NA,.after = "facility") %>%
      dplyr::mutate(fy = lubridate::quarter(date, with_year = TRUE, fiscal_start = 10) %>% stringr::str_sub(., 1, 4))

  #standardize variable order
    df <- order_vars(df)

  #export
    export_hfr(df, folderpath_output)

  return(df)

}
