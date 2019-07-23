#' Import and Structure Haiti
#'
#' @description This function will be used to take the Haiti HFR
#' dataset and structure in a uniform way to be tidy and align with other
#' OU datasets. Only selected indicators will be imported and transformed
#' based on OHA's needs. The data is sourced from Haiti's partner weekly
#' reporting template files.
#'
#' @param filepath filepath to the output file
#' @param folderpath_output if a txt output is desired, provide the full path to the folder
#'
#' @export
#'
#' @examples
#' \dontrun{
#' #structure output for Hait
#'   path <- "~/WeeklyData/Raw/HTI_monthly_data.xlsx"
#'   structure_hti(path)
#' #structure output for Haiti & export to txt file
#'   structure_hti(path, "~/WeeklyData/Output") }

structure_hti <- function(filepath, folderpath_output = NULL){

  #import
    df <- readxl::read_excel(filepath, sheet = "Weekly Data_Entry",
                             skip = 8, col_types = "text")

    meta <- c("Facility", "Agency", "Clinical Partner", "Department", "Arrondissement")

  #limit variables and reshape, also removing calculated and unnecessary components
    df <- df %>%
      dplyr::select(meta, dplyr::matches("^(HTS_TST|TX_NEW|TX_CURR|MMD - 6)")) %>%
      reshape_long(meta) %>%
      dplyr::filter(stringr::str_detect(ind, "Target|Results|Total|Achievement|Baseline",
                                        negate = TRUE))

  #limit to just USAID
    df <- dplyr::filter(df, Agency == "USAID")

  #breakout date from indicator & add FY
    df <- df %>%
      tidyr::separate(ind, c("indicator", "date"), sep = "\\r\\n|[:space:]{2,}") %>%
      dplyr::mutate(indicator = ifelse(indicator == "MMD - 6 months", "TX_MMD", indicator),
                    disaggregate = ifelse(indicator == "MMD", "Period", "Total Numerator"),
                    otherdisaggregate = dplyr::case_when(indicator == "TX_MMD" ~ "6 months or more"),
                    date = stringr::str_remove(date, "-.*$") %>%
                      paste0(., ", 2019") %>% lubridate::mdy(),
                    fy = lubridate::quarter(date, with_year = TRUE, fiscal_start = 10) %>%
                      stringr::str_sub(., 1, 4))

  #clean up geo and other names
    df <- df %>%
      dplyr::rename(fundingagency = Agency,
                    psnu = Department,
                    community = Arrondissement,
                    partner = `Clinical Partner`,
                    facility = Facility)
  #add ou
    df <- add_ou(df, "Haiti")

  #add reporting frequency
    df <- tibble::add_column(df, reporting_freq = "Weekly",
                             .before = "date")

  #standardize variable order
    df <- order_vars(df)

  #export
    export_hfr(df, folderpath_output)

  invisible(df)
}
