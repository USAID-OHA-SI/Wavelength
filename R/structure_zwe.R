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
      stringr::str_subset(pattern = "Weeks_June10_July1")

    df <- readxl::read_excel(filepath, sheet, col_types = "text")

#clean var names and values

    df <- df %>%
      dplyr::rename(partner = `Partner name`,
                    operatingunit = `Operating Unit`,
                    reporting_freq = Reporting_Frequency,
                    mechanismid = Mechanism_ID,
                    facility = SiteName,
                    indicator = Indicator,
                    agecoarse = Agecoarse,
                    sex = Sex,
                    value = Value,
                    date = Reporting_Week_Starting,
                    psnu = PSNU,
                    snu1 = SNU1) %>%
      dplyr::mutate_at(dplyr::vars(value, date),as.numeric) %>%
      dplyr::filter(value > 0,
                  agecoarse != "All") %>%
      tibble::add_column(disaggregate = "",
                       fundingagency = "USAID") %>%
      dplyr::mutate(date = lubridate::as_date(date, origin = "1899-12-30"),
                    fy = lubridate::year(date),
                    sex = dplyr::recode(sex, "m" = "Male", "f" = "Female"))


#tidy indicator

    df <- df %>%
      dplyr::filter(indicator %in% c("HTS_TST", "HTS_TST_POS", "PrEP_NEW", "TX_NEW", "TX_CURR", "VMMC_CIRC"))

# add columns
    df <- df %>%
    dplyr::mutate(disaggregate = "Age/Sex")


    #standardize variable order
    df <- order_vars(df)

    #export
    export_hfr(df, folderpath_output)

    invisible(df)

}


