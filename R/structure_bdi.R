#' Import and Structure Burundi
#'
#' @description This function will be used to take the weekly Burundi
#' dataset and structure in a uniform way to be tidy and align with other
#' OU datasets. Only selected indicators will be imported and transformed
#' based on OHA's needs. The data is sourced from Burundi's partner weekly
#' reporting template files.
#'
#' @param filepath filepath to the output file
#' @param folderpath_output if a txt output is desired, provide the full path to the folder
#'
#' @export
#'
#' @examples
#' \dontrun{
#' #structure output for Burundi
#'   path <- "~/WeeklyData/Raw/BDI_monthly_data.xlsx"
#'   structure_hti(path)
#' #structure output for Burundi & export to txt file
#'   structure_hti(path, "~/WeeklyData/Output") }

structure_bdi <- function(filepath, folderpath_output = NULL){

  sht_exclude <- c("Summary")

  #import
    df <- filepath %>%
      readxl::excel_sheets() %>%
      setdiff(sht_exclude) %>%
      purrr::set_names() %>%
      purrr::map_dfr(~readxl::read_excel(filepath, sheet = .x, skip = 2, col_types = "text"),
                     .id = "date")


  #keep only select variables (no PMTCT & totals for HTS & TX) & rename
    df <- df %>%
      dplyr::select(psnu = Province,
                    facility = `Health Center (CDS)`,
                    date,
                    HTS_TST = `Total tested`,
                    HTS_TST_POS = `Total Tested POS`,
                    TX_NEW = `Total new cases on ART`,
                    TX_MMD = `Number of PLHIV on MMP ( new case in the week)`)
  #reshape
    meta <- c("psnu", "facility", "date")
    df <- df %>%
      reshape_long(meta) %>%
      dplyr::rename(indicator = ind)

  #adjust date
    df <- df %>%
      dplyr::mutate(date = stringr::str_replace(date, "-[:digit:]{1,2}", ",2019"),
                    date = lubridate::mdy(date)) %>%
      assign_pds()

  #add ou
    df <- add_ou(df, "Burundi")

  #add partner
    df <- dplyr::mutate(primepartner = "RAGF")

  #standardize variable order
    df <- order_vars(df)

  #export
    export_hfr(df, folderpath_output)

  return(df)

}
