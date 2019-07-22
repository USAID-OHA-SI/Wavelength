#' Import and Structure Ukraine
#'
#' @description This function will be used to take the weekly Ukraine
#' dataset and structure in a uniform way to be tidy and align with other
#' OU datasets. Only selected indicators will be imported and transformed
#' based on OHA's needs. The data is sourced from Ukraine's partner weekly
#' reporting template files.
#'
#' @param filepath filepath to the output file
#' @param folderpath_output if a txt output is desired, provide the full path to the folder
#'
#' @export
#'
#' @examples
#' \dontrun{
#' #structure output for UKR
#'   path <- "~/WeeklyData/Raw/UKR_partner_x.xlsx"
#'   structure_ukr(path)
#' #structure output for UKR & export to txt file
#'   structure_ukr(path, "~/WeeklyData/Output") }

structure_ukr <- function(filepath, folderpath_output = NULL){

  if(stringr::str_detect(filepath, "HealthLink")){

    #import
      df <- readxl::read_excel(filepath, col_types = "text")
    #clean up date/val/fy and keep only mechid, removing mech name
      df <- df %>%
        dplyr::mutate(date = lubridate::as_date(as.integer(date), origin = "1899-12-30"),
                      val = as.numeric(val),
                      fy = as.integer(fy),
                      disaggregate = "Age/Sex") %>%
        tidyr::separate(mechanismid, c("mechanismid", NA), sep = " -")

  } else if(stringr::str_detect(filepath, "Serving Life")) {

    #import
      df <- filepath %>%
        readxl::excel_sheets() %>%
        purrr::set_names() %>%
        purrr::map_dfr( ~readxl::read_excel(filepath, .x, col_types = "text"), .id = "indicator")

    #remove total
      df <- dplyr::select(df, -dplyr::contains("Month"))

    #reshape long
      meta <- dplyr::select(df, -dplyr::contains("week")) %>% names()
      df <- df %>%
        reshape_long(meta) %>%
        dplyr::rename(date = ind)

    #adjust indicator name, removing pd from end
      df <- dplyr::mutate(df, indicator = stringr::str_remove(indicator, "_[:digit:]{1,}$"))

    #rename columns
      df <- df %>%
        dplyr::rename_all(~stringr::str_remove(., " ") %>% tolower) %>%
        dplyr::rename(operatingunit = operationunit,
                      partner = partnername,
                      orgunituid = facilityuid,
                      facility = facilityname,
                      agecoarse = age)

    #clean up date
      df <- df %>%
        tidyr::separate(date, c(NA, "date"), sep = "\\(") %>%
        dplyr::mutate(date = lubridate::as_date(date),
                      fy = lubridate::quarter(date, with_year = TRUE, fiscal_start = 10) %>%
                        stringr::str_sub(., 1, 4))

    #add reporting frequency
      df <- dplyr::mutate(df, reporting_freq = "Weekly")

    #add disaggregate
      df <- dplyr::mutate(df, disaggregate = "Age/Sex")

    #standardize variable order
      df <- order_vars(df)

  } else {

    return(NULL)

  }

  #export
    export_hfr(df, folderpath_output)

  invisible(df)

}

