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
  sheets <- c("HTS_TST_POS", "TX_NEW", "TX_NEW_SAMEDAY", "cLTFU", "uLTFU", "TX_CURR_28") %>% purrr::set_names()

  df <- purrr::map_dfr(.x = sheets,
                       .f = ~ readxl::read_excel(filepath, sheet = .x, col_types = "text"))
  #check structure
  #TODO add assert check to make sure structure stays the same

  #reshape long
    meta <- dplyr::select(df, FundingAgency:Facility) %>% names()
    df <- df %>%
      reshape_long(meta) %>%
      dplyr::rename(date = ind)

  #fix issues with IM and excel date that end with decimal
    df <- dplyr::mutate_at(df, dplyr::vars(MechanismID, date), ~ as.integer(.) %>% as.character())

  #rename geography for uniformity & rename lower
    df <- df %>%
      dplyr::rename(snu1 = Province,
                    psnu = District,
                    community = `Sub-district`) %>%
      dplyr::select(-`Siyenza Sites`, -TIER) %>%
      dplyr::rename_all(~tolower(.))

  #adjust from Excel date format
    df <- df %>%
      dplyr::mutate(date = lubridate::as_date(as.integer(date), origin = "1899-12-30"),
                    fy = lubridate::quarter(date, with_year = TRUE, fiscal_start = 10) %>% stringr::str_sub(., 1, 4)) %>%
      dplyr::select(-val, dplyr::everything())

  #add disaggregate
    df <- df %>%
      tibble::add_column(disaggregate = "Total Numerator", .after = "indicator")

  #add operatingunit
    df <- add_ou(df, "South Africa")

  #add reporting frequency
    df <- tibble::add_column(df, reporting_freq = as.character(NA),
                             .after = "facility") %>%
      dplyr::mutate(reporting_freq = dplyr::case_when(indicator %in% c("HTS_TST_POS", "TX_NEW", "TX_NEW_SAMEDAY") ~ "daily",
                                                      indicator %in% c("cLTFU", "uLTFU") ~ "weekly",
                                                      indicator %in% c("TX_CURR_28") ~ "biweekly"))
    #TODO arrange variable order

  #export
    export_hfd(df, folderpath_output)

    return(df)
}
