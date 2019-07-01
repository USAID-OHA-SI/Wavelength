#' Import and Structure Ethiopia
#'
#' @description This function will be used to take the Ethiopia HFR
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
#' #structure output for Ethiopia
#'   path <- "~/WeeklyData/Raw/ETH_Weekly.xlsx"
#'   structure_eth(path)
#' #structure output for Ethiopia & export to txt file
#'   structure_eth(path, "~/WeeklyData/Output") }

structure_eth <- function(filepath, folderpath_output = NULL){

  #import sheets, 2 partners
    rel_sheets <- c("PSI_KP", "Project_Hope_CHCT")
    df <- purrr::map_dfr(.x = rel_sheets, .f = ~readxl::read_excel(filepath, skip = 4, sheet = .x, col_types = "text"))

  #keep only relevant columns
    meta <- c("DATE_START", "PARTNER", "IM_ID", "AGENCY",
              "OU", "REGION", "DISTRICT", "SITE", "SITE_ID")
    ind <- c("HTS_M", "HTS_F", "HTS_PEDS",
             "POS_M", "POS_F", "POS_PEDS",
             "TX_NEW_M", "TX_NEW_F", "TX_NEW_PEDS",
             "TX_CURR", "PrEP_NEW",
             "MMS_1", "MMS_2", "MMS_3",
             "HTS_REF_FAC", "HTS_REF_POS_FAC")
    df <- dplyr::select(df, meta, ind)

  #reshape long
     df <- reshape_long(df, meta)

  #clean up column names
    df <- df %>%
      dplyr::rename_all(tolower) %>%
      dplyr::rename(date = date_start,
                    mechanismid = im_id,
                    fundingagency = agency,
                    operatingunit = ou,
                    snu1 = region,
                    psnu = district,
                    facility = site,
                    orgunituid = site_id)
  #fix types
    df <- df %>%
      dplyr::mutate(date = as.Date(as.double(date), origin = "1899-12-30"),
                    fy = lubridate::quarter(date, with_year = TRUE, fiscal_start = 10) %>%
                      stringr::str_sub(., 1, 4),
                    val = as.double(val))

  #fix date to align with HFR (off by 1 day)
    df <- dplyr::mutate(df, date = date + 1)

  #clean indicators/disaggs
    df <- df %>%
      dplyr::left_join(ind_map_eth, by = "ind") %>%
      dplyr::select(-ind) %>%
      dplyr::select(-val, dplyr::everything())

  #add reporting frequency
    df <- tibble::add_column(df, reporting_freq = "Weekly",
                             .after = "facility")
  #standardize variable order
    df <- order_vars(df)

  #export
    export_hfr(df, folderpath_output)

  invisible(df)
}
