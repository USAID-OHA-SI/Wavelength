#' Import and Structure Uganda
#'
#' @description This function will be used to take the weekly Uganda
#' dataset and structure in a uniform way to be tidy and align with other
#' OU datasets. Only selected indicators will be imported and transformed
#' based on OHA's needs. The data is sourced from Uganda's partner weekly
#' reporting template files.
#'
#' @param filepath filepath to the output file
#' @param folderpath_output if a txt output is desired, provide the full path to the folder
#'
#' @export
#'
#' @examples
#' \dontrun{
#' #structure output for UGA
#'   path <- "~/WeeklyData/Raw/UGA_partner_x.xlsx"
#'   structure_uga(path)
#' #structure output for UGA & export to txt file
#'   structure_uga(path, "~/WeeklyData/Output") }

structure_uga <- function(filepath, folderpath_output = NULL){

  #import dataset
    df <- readr::read_csv(filepath, col_types = readr::cols(.default = "c"), progress = FALSE) %>%
      dplyr::mutate(value = as.numeric(value))

  #check structure
    #TODO add assert check to make sure structure stays the same

  #map indicator/disagg names onto dataframe
    df <- dplyr::full_join(df, ind_map_uga,
                           by = c("DataElement.name" = "indicator_orig",
                                  "DISAGG" = "disaggregate_orig"))

  #remove indicators/columns OHA is not collecting
    df <- df %>%
      dplyr::select(reporting_pd = `Report period`,
                    snu1 = region, psnu = district,
                    community = subcounty, facility = Outlet,
                    orgunituid = Outlet.uid, partner = IM,
                    indicator:modality, val = value) %>%
      dplyr::filter(!is.na(indicator),
                    !val %in% c(0, NA))

  #add HTS
    df_hts <- df %>%
      dplyr::filter(indicator %in% c("HTS_TST_NEG", "HTS_TST_POS")) %>%
      dplyr::mutate(indicator = "HTS_TST") %>%
      dplyr::group_by_if(is.character) %>%
      dplyr::summarise_at(dplyr::vars(val), sum, na.rm = TRUE) %>%
      dplyr::ungroup()

    df <- df %>%
      dplyr::bind_rows(df_hts) %>%
      dplyr::filter(indicator != "HTS_TST_NEG")

    rm(df_hts)
  #add operatingunit
    df <- add_ou(df, "Uganda")

  #add reporting frequency
    df <- tibble::add_column(df, reporting_freq = "Weekly",
                           .after = "partner")

  #break out reporting pd & add month and FY
    df <- df %>%
      tidyr::separate(reporting_pd, c("date", NA), sep = "/") %>%
      dplyr::mutate(date = lubridate::as_date(date),
                    fy = lubridate::quarter(date, with_year = TRUE, fiscal_start = 10) %>%
                      stringr::str_sub(., 1, 4)) %>%
      dplyr::select(-val, dplyr::everything())

  #cleanup indicator and disaggs
    components <- c("indicator", "disaggregate", "agecoarse", "agesemifine", "sex", "modality")
    df <- df %>%
      dplyr::mutate_at(dplyr::vars(dplyr::one_of(components)), ~ dplyr::na_if(., "NA")) %>%
      dplyr::mutate(sex = ifelse(sex == "Peds", "Unknown", sex))

  #standardize variable order
    df <- order_vars(df)

  #export
    export_hfr(df, folderpath_output)

  invisible(df)
}
