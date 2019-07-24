#' Import and Structure Malawi
#'
#' @description This function will be used to take the Malawi HFR
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
#' #structure output for Malawi
#'   path <- "~/WeeklyData/Raw/MWI_Weekly.xlsx"
#'   structure_eth(path)
#' #structure output for Malawi & export to txt file
#'   structure_eth(path, "~/WeeklyData/Output") }

structure_mwi <- function(filepath, folderpath_output = NULL){

  if(stringr::str_detect(filepath, "Baylor")){

    #import; headers are across 2 rows, so we need to identify and create them
    headers <- identify_headers_mwi(filepath)
    df <- readxl::read_excel(filepath, sheet = "data by week April& May",
                             skip = 2, col_names = headers, col_types = "text")

    #reshape
    meta <- c("DISTRICT", "SITE", "Mechanism Number", "orgUnit")
    df <- df %>%
      dplyr::select(-c("YEAR", "MONTH", "Facility ID")) %>%
      reshape_long(meta)

    #rename variables
    df <- df %>%
      dplyr::rename_all(tolower) %>%
      dplyr::rename(psnu = district,
                    facility = site,
                    mechanismid = `mechanism number`,
                    orgunituid = orgunit)

    #add fundingagency
    df <- dplyr::mutate(df, fundingagency = "USAID")

    #clean up date/indicator
    df <- df %>%
      tidyr::separate(ind, c("pd", "indicator"), sep = "\\.") %>%
      dplyr::mutate(pd = stringr::str_remove_all(pd, "^.*(; |: |\\()") %>%
                      stringr::str_remove("\\)")) %>%
      tidyr::separate(pd, c("date", "end"), sep = "( |  )-( |)") %>%
      dplyr::mutate_at(dplyr::vars(date, end), lubridate::dmy) %>%
      dplyr::filter(end != as.Date("2019-04-30"), date != as.Date("2019-05-01")) %>%
      dplyr::select(-end) %>%
      dplyr::mutate(fy = lubridate::quarter(date, with_year = TRUE, fiscal_start = 10) %>%
                      stringr::str_sub(., 1, 4))

    #add disaggregate
      df <- dplyr::mutate(df, disaggregate = "Total Numerator")

  } else if (stringr::str_detect(filepath, "PIH")){

    #import
      sheets <- c("HTS_TST", "TX_NEW")
      df <- purrr::map_dfr(sheets, ~ readxl::read_excel(filepath, sheet = .x, col_types = "text"))

    #reshape
      meta <- c("PrimePartner", "Region", "FundingAgency", "District", "Site Name", "Month", "Week")
      df <- df %>%
        dplyr::select(-c(Zone, Facility_ID, HTS_TST, HTS_TST_POS, TX_NEW)) %>%
        reshape_long(meta) %>%
        dplyr::rename_all(tolower) %>%
        dplyr::rename(partner = primepartner,
                      psnu = district,
                      snu1 = region,
                      facility = `site name`)
    #apply indicator mapping
      df <- df %>%
        dplyr::left_join(ind_map_mwi, by = "ind") %>%
        dplyr::select(-ind)

    #creat HTS_TST
      group_vars <- dplyr::select(df, -val) %>% names()
      df_hts <- df %>%
        dplyr::filter(indicator %in% c("HTS_TST_POS", "HTS_TST_NEG")) %>%
        dplyr::mutate(indicator == "HTS_TST",
                      resultstatus = NA) %>%
        dplyr::group_by_at(group_vars) %>%
        dplyr::summarise_at(dplyr::vars(val), sum, na.rm = TRUE) %>%
        dplyr::ungroup()

      df <- df %>%
        dplyr::filter(indicator != "HTS_TST_NEG") %>%
        dplyr::bind_rows(df_hts)

    #create date
      df <- df %>%
        dplyr::filter(!is.na(week)) %>%
        dplyr::mutate(date = paste(month, stringr::str_extract(week, "^[:digit:]{1,2}"), 2019) %>%
                        lubridate::mdy() + 2) %>%
        dplyr::select(-month, -week) %>%
        assign_pds()
  }

  #add OU
    df <- add_ou(df, "Malawi")

  #add reporting frequency
    df <- tibble::add_column(df, reporting_freq = "Weekly",
                             .after = "facility")

  #standardize variable order
    df <- order_vars(df)

  #export
    export_hfr(df, folderpath_output)

    invisible(df)
  }
