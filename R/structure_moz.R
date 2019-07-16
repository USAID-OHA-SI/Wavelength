#' Import and Structure Mozambique
#'
#' @description This function will be used to take the Mozambique HFR
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
#' #structure output for Mozambique
#'   path <- "~/WeeklyData/Raw/MOZ_Weekly.xlsx"
#'   structure_moz(path)
#' #structure output for Mozambique & export to txt file
#'   structure_moz(path, "~/WeeklyData/Output") }

structure_moz <- function(filepath, folderpath_output = NULL){

  #import sheets
    df <- filepath %>%
      readxl::excel_sheets() %>%
      purrr::map_dfr( ~ readxl::read_excel(filepath, sheet = .x, col_types = "text"))

  #clean up
    meta <- c("Date", "Datim ID", "Partner (COP19)", "Mechanism ID",
              "Agency (COP19)", "Province", "District", "Site Name")
    df <- df %>%
      dplyr::select(-c(`Sisma ID`, AJUDA, DataSource), -dplyr::contains("NET_NEW")) %>%
      reshape_long(meta)

  #rename columns
    df <- df %>%
      dplyr::rename_all(~ stringr::str_remove_all(. ," ") %>% tolower) %>%
      dplyr::rename(orgunituid = datimid,
                    partner = `partner(cop19)`,
                    fundingagency = `agency(cop19)`,
                    snu1 = province,
                    psnu = district,
                    facility = sitename)

  #adjust variables and disaggregates
    df <- df %>%
      tidyr::separate(ind, c("indicator", "agecoarse"), sep = " ", fill = "right") %>%
      dplyr::mutate(indicator = dplyr::case_when(indicator == "3MD" ~ "TX_MMD",
                                                 indicator == "VMMC" ~ "VMMC_CIRC",
                                                 TRUE ~ indicator),
                    otherdisaggregate = dplyr::case_when(indicator == "TX_MMD" ~ "3 months"),
                    disaggregate = dplyr::case_when(indicator == "TX_MMD" ~ "Period",
                                                    !is.na(agecoarse) ~ "Age/Sex",
                                                    TRUE ~ "Total Numerator"))

  #fix date format
    df <- df %>%
      dplyr::mutate(date = lubridate::as_date(as.integer(date), origin = "1899-12-30"),
                    fy = lubridate::quarter(date, with_year = TRUE, fiscal_start = 10) %>%
                      stringr::str_sub(., 1, 4)) %>%
      dplyr::select(date, fy, dplyr::everything())

  #add operating unit
    df <- add_ou(df, "Mozambique")

  #add reporting frequency
    df <- tibble::add_column(df, reporting_freq = "Weekly",
                           .after = "facility")
  #standardize variable order
    df <- order_vars(df)

  #export
    export_hfr(df, folderpath_output)

  invisible(df)
}

