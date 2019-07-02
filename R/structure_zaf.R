#' Import and Structure South Africa
#'
#' @description This function will be used to take the daily/weekly SA
#' dataset and structure in a uniform way to be tidy and align with other
#' partners/OU datasets. Only selected indicators will be imported and transformed
#' based on OHA's needs. The data is sourced from South Africa's partner's monthly
#' reporting template files.
#'
#' @param filepath filepath to the output file
#' @param folderpath_output if a txt output is desired, provide the full path to the folder
#'
#' @export
#'
#' @examples
#' \dontrun{
#' #structure output for ZAF
#'   path <- "~/WeeklyData/Raw/ZAF_partner_x.xlsx"
#'   structure_zaf(path)
#' #structure output for ZAF & export to txt file
#'   structure_zaf(path, "~/WeeklyData/Output") }

structure_zaf <- function(filepath, folderpath_output = NULL){

  #import
    df <- readr::read_csv(filepath, col_types = c(.default = "c"))

  #limit columns & conver to lower case
    df <- df %>%
      dplyr::select(FundingAgency:`Sub-district`, Facility, date = Week_Start, Indicator, val = Value) %>%
      dplyr::rename_all(tolower)

  #filter to select indicators
    ind_sel <- c("HTS_TST","HTS_TST_POS", "TX_NEW", "TX_CURR_28")
    df <- df %>%
      dplyr::filter(indicator %in% ind_sel) %>%
      dplyr::mutate(indicator = stringr::str_remove(indicator, "_28"))

  #align geography
    df <- df %>%
      dplyr::rename(snu1 = province,
                    psnu = district,
                    community = `sub-district`)

  #add operating unit
    df <- add_ou(df, "South Africa")

  #format date and value
    df <- df %>%
      dplyr::mutate(date = lubridate::mdy(date),
                    fy = lubridate::quarter(date, with_year = TRUE, fiscal_start = 10) %>%
                      stringr::str_sub(., 1, 4),
                    val = as.numeric(val))

  #add disaggregate
    df <- tibble::add_column(df, disaggregate = "Total Numerator", .after = "indicator")

  #add reporting frequency
    df <- dplyr::mutate(df, reporting_freq = ifelse(indicator == "TX_CURR", "Bi-weekly", "Weekly"))

  #standardize variable order
    df <- order_vars(df)

  #export
    export_hfr(df, folderpath_output)

  invisible(df)
}
