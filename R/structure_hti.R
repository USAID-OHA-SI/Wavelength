#' Import and Structure Haiti
#'
#' @description This function will be used to take the monthly Haiti
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

  #import; headers are across 2 rows, so we need to identify and create them
    headers <- identify_headers_hti(filepath, "ALL")
    df <- readxl::read_excel(filepath, sheet = "ALL", skip = 3, col_names = headers, col_types = "text") %>%
      dplyr::slice(3:nrow(.)) #remove header rows

  #keep only select variables
    df <- df %>%
      dplyr::select(dplyr::ends_with("NA"), dplyr::matches("HTS_TST|TX_"),
                    -dplyr::contains("Comments"), -`TX_CURR (previous month).Sept`)

  #reshape long
    meta <- dplyr::select(df, dplyr::ends_with("NA")) %>% names()
    df <- reshape_long(df, meta)

  #clean up var names and geography
    df <-df %>%
      dplyr::rename_all( ~ stringr::str_remove(., "\\.NA$")) %>%
      dplyr::rename(psnu = Department,
                    community = Arrondissement,
                    partner = `Clinical Partner`,
                    facility = Facility) %>%
      dplyr::select(-`Sante Regional Office`, -`MSPP Facility Code`) %>%
      dplyr::glimpse()

  #breakout indicator and month 7 clean up month
    df <- tidyr::separate(df, ind, c("indicator", "month"), sep = "\\.")

    month_map <- tibble::tibble(month = format(ISOdate(2019,1:12,1),"%b"),
                                month_full = format(ISOdate(2019,1:12,1),"%B"))

    df <- df %>%
      dplyr::left_join(month_map, by = "month") %>%
      dplyr::mutate(month = month_full) %>%
      dplyr::select(-month_full)

  #clean up indicators and disaggs
    df <- df %>%
      dplyr::mutate(indicator = stringr::str_replace(indicator, "Index HTS_TST", "HTS_INDEX"),
                    indicator = stringr::str_replace(indicator, "ML ", "ML.")) %>%
      tidyr::separate(indicator, c("indicator", "otherdisaggregate"), sep = "\\.", fill = "right") %>%
      dplyr::mutate(disaggregate = ifelse(indicator == "TX_ML", "Outcome", "Total Numerator"))

  #add ou
    df <- add_ou(df, "Haiti")

  #add reporting frequency
    df <- tibble::add_column(df, reporting_freq = "Monthly",
                             .after = "community")

  #TODO arrange variable order

  #export
    export_hfd(df, folderpath_output)
}
