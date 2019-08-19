#' Import and Structure Tanzania
#'
#' @description This function will be used to take the monthly Tanzania
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
#' #structure output for Tanzania
#'   path <- "~/WeeklyData/Raw/All Monthly Raw Data in given timeframe.xlsx"
#'   structure_tza(path)
#' #structure output for Tanzania & export to txt file
#'   structure_tza(path, "~/WeeklyData/Output") }

structure_tza <- function(filepath, folderpath_output = NULL){

  #import
    df <- purrr::map_dfr(.x = readxl::excel_sheets(filepath),
                         .f = ~ readxl::read_xlsx(filepath, sheet = .x,
                                                  skip = ifelse(.x == "Weekly USAID data", 2, 4),
                                                  col_type = "text"))

  #filter out unnessary variables
    vars <- setdiff(names(df), c("DATE_END", "...3", "VMMC_CIRC_PED...30", "VMMC_CIRC_M...31", "Month_Num"))
    df <- dplyr::select_at(df, vars)

  #fix naming issue for weekly
    df <- df %>%
      dplyr::rename(PrEP_NEW_M = PrEP_NEW_M...24,
                    PrEP_NEW_F = PreP_NEW_F...25,
                    PrEP_NEW_PEDS_M = PrEP_NEW_M...26,
                    PrEP_NEW_PEDS_F = PreP_NEW_F...27,
                    VMMC_CIRC_PED = VMMC_CIRC_PED...28,
                    VMMC_CIRC_M = VMMC_CIRC_M...29)

  #fix site_id v datim_id
      df <- df %>%
        dplyr::mutate(DATIM_ID = ifelse(!is.na(SITE_ID), SITE_ID, DATIM_ID)) %>%
        dplyr::select(-SITE_ID)

  #fix date (one day off, start on Sundays, one year = 2016)
    df <- df %>%
      dplyr::mutate(date = as.Date(as.double(DATE_START), origin = "1899-12-30") + 2,
                    date = dplyr::case_when(lubridate::year(date) == 2016 ~ date + lubridate::years(3),
                                            TRUE ~ date),
                    date = lubridate::floor_date(date, unit = "week", week_start = 1))

  #fix for month
    df <- df %>%
      dplyr::mutate(Month = dplyr::case_when(Month == "June" ~ lubridate::ymd("2019-06-03")),
                    date = ifelse(!is.na(Month), as.Date(Month), as.Date(date)),
                    date = lubridate::as_date(date)) %>%
      dplyr::select(-Month, - DATE_START) %>%
      dplyr::select(date, DATIM_ID, dplyr::everything())

  #munge and reshape long
    meta <- dplyr::select(df, date:SITE) %>% names()
    df <- reshape_long(df, meta)

  #clean names
    df <- df %>%
      dplyr::rename(facility = SITE,
                    orgunituid = DATIM_ID,
                    snu1 = REGION,
                    psnu = COUNCIL,
                    community = WARD,
                    fundingagency = AGENCY,
                    indicator = ind) %>%
      dplyr::select(-DISTRICT) %>%
      dplyr::rename_all(tolower) #%>%
      #dplyr::mutate(facility = stringr::str_remove_all(facility, " -.*$| \\."))

  #map indicator name on & filter to just OHA select values
    df <- df %>%
      dplyr::mutate(sex = stringr::str_extract(indicator, "M|F"),
                    agecoarse = ifelse(stringr::str_detect(indicator, "PEDS"), "<15", "15+"),
                    sex = dplyr::recode(sex, "M" = "Male", "F" = "Female"),
                    indicator = stringr::str_remove_all(indicator, "_(PEDS|A|M|F)"),
                    indicator = dplyr::recode(indicator, "HTS" = "HTS_TST", "POS" = "HTS_TST_POS"))

  #add reporting pd
    df <- dplyr::mutate(df, reporting_freq = ifelse(indicator == "TX_CURR", "Monthly", "Weekly"))

  #add FY
    df <- assign_pds(df)

  #add operatingunit
    df <- add_ou(df, "Tanzania")

  #add disaggregate
    df <- dplyr::mutate(df,  disaggregate = "Age/Sex")

  #standardize variable order
    df <- order_vars(df)

  #export
    export_hfr(df, folderpath_output)

  invisible(df)

}
