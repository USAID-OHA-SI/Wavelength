#' Import and Structure Zambia
#'
#' @description This function will be used to take the Zambia HFR
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
#' #structure output for Zambia
#'   path <- "~/WeeklyData/Raw/ZMB_Weekly.xlsx"
#'   structure_zmb(path)
#' #structure output for Zambia & export to txt file
#'   structure_zmb(path, "~/WeeklyData/Output") }

structure_zmb <- function(filepath, folderpath_output = NULL){

  #import data
    sheet <- filepath %>%
      readxl::excel_sheets(.) %>%
      stringr::str_subset(pattern = "Submission|Week")

    df <- readxl::read_excel(filepath, sheet, skip = 1, col_types = "text")

  #remove Total columns
    df <- dplyr::select(df, -dplyr::contains("Total"))

  #reshape long
    meta <- c("Mechanism ID", "Partner", "OU",
              "Province", "District", "Facility",
              "Facility_UID", "Reporting Period")

    df <- reshape_long(df, meta)
    rm(meta)

  #clean up date & add FY
    df <- df %>%
      dplyr::rename(date = `Reporting Period`) %>%
      dplyr::mutate(date = lubridate::as_date(as.integer(date), origin = "1899-12-30")) %>%
      dplyr::select(date, dplyr::everything()) %>%
      assign_pds()


  #separate indicator and disaggs
    df <- df %>%
      dplyr::mutate(ind = stringr::str_replace(ind, "HTS(\\.|_)TST\\.POS", "HTS_TST_POS"),
                    ind = stringr::str_replace(ind, "S<", "S\\.<"),
                    ind = stringr::str_replace(ind, "MMD\\.","TX_MMD\\.\\.\\."))

    df <- df %>%
      tidyr::separate(ind, c("indicator", "agecoarse", "sex", "otherdisaggregate"), sep = "\\.", fill = "right") %>%
      dplyr::mutate(sex = dplyr::case_when(sex == "M" ~ "Male",
                                           sex == "F" ~ "Female"),
                    agecoarse = dplyr::na_if(agecoarse, ""),
                    otherdisaggregate = stringr::str_replace(otherdisaggregate, "(months|month)", " \\1"))

  #add disaggregate column
    df <- dplyr::mutate(df, disaggregate = ifelse(indicator == "TX_MMD", "Period", "Age/Sex"))

  #clean up headers
    df <- df %>%
      dplyr::rename(mechanismid = `Mechanism ID`,
                    operatingunit = OU,
                    snu1 = Province,
                    psnu = District,
                    orgunituid = Facility_UID) %>%
      dplyr::rename_all(tolower)

  #add funding agency
    df <- tibble::add_column(df, fundingagency = "USAID",
                           .after = "operatingunit")
  #add reporting frequency
    df <- tibble::add_column(df, reporting_freq = "Weekly",
                           .after = "facility")
  #standardize variable order
    df <- order_vars(df)

  #export
    export_hfr(df, folderpath_output)

  invisible(df)
}
