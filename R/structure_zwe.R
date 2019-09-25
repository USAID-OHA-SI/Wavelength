# import and structure Zimbabwe

#' Import and Structure Zimbabwe
#'
#' @description This function will be used to take the Zimbabwe HFR
#' dataset and structure in a uniform way to be tidy and align with other
#' OU datasets. Only selected indicators will be imported and transformed
#' based on OHA's needs. The data is sourced from Zimbabwe's partner weekly
#' reporting template files.
#'
#' @param filepath filepath to the output file
#' @param folderpath_output if a txt output is desired, provide the full path to the folder
#'
#' @export
#'
#' @examples
#' \dontrun{
#' #structure output for Zimbabwe
#'   path <- "~/WeeklyData/Raw/zwe_monthly_data.xlsx"
#'   structure_zwe(path)
#' #structure output for Zimbabwe & export to txt file
#'   structure_zwe(path, "~/WeeklyData/Output") }
#'
structure_zwe <- function(filepath, folderpath_output = NULL){

  # import
    sheet <- filepath %>%
      readxl::excel_sheets(.) %>%
      stringr::str_subset(pattern = "Sept HFR")

    df <- readxl::read_excel(filepath, sheet, col_types = "text")

    df <- df %>%
      dplyr::rename_all(tolower) %>%
      dplyr::rename("2019-08-05" = `43593`,
                    "2019-08-12" = `43807`,
                    "2019-08-19" = `19/08/2019`,
                    "2019-08-26" = `26/08/2019`,
                    operatingunit = `operating unit`,
                    facility = `community/facility name`,
                    mechanismid = mechanism_id)

    df <- df %>%
      tidyr::gather(date, val, `2019-08-05`:`2019-08-26`)

    df <- df %>%
      dplyr::mutate(date = lubridate::as_date(date),
                    val = as.numeric(val),
                    indicator = dplyr::case_when(indicator == "PrEP_New" ~ "PrEP_NEW",
                                                 TRUE ~ indicator),
                    sex = dplyr::case_when(sex == "M/F" ~ "Unknown",
                                           sex == "FeMale" ~ "Female",
                                           TRUE ~ sex)) %>%
      dplyr::select(-`reporting frequency`, -`partner name`) %>%
      dplyr::filter(val != 0) %>%
      assign_pds()

  #standardize variable order
    df <- order_vars(df)

  #export
    export_hfr(df, folderpath_output)

    invisible(df)
}

