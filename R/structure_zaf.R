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

  if(stringr::str_detect(filepath, "URC")){

    #import
      df <- readxl::read_xlsx(filepath, col_types = "text")

    #convert columns to lower case and reshape
      df <- dplyr::rename_all(df, tolower)
      meta <- dplyr::select(df, fundingagency:facility) %>% names()
      df <- reshape_long(df, meta) %>%
        dplyr::rename(date = ind)
    #format date
      df <- dplyr::mutate(df, date = lubridate::as_date(as.integer(date), origin = "1899-12-30"))
  } else {
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

    #format date
      df <- dplyr::mutate(df, date = lubridate::mdy(date))

  }

  #align geography
    df <- df %>%
      add_ou("South Africa") %>%
      dplyr::rename(snu1 = province,
                    psnu = district,
                    community = `sub-district`)

  #format date and value
    df <- df %>%
      dplyr::mutate(fy = lubridate::quarter(date, with_year = TRUE, fiscal_start = 10) %>%
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
