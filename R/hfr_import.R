#' Import template sheet(s)
#'
#' @param filepath filepath to sumbitted template
#'
#' @export
#'
#' @examples
#' \dontrun{
#' #identify whether template is long or wide
#'   filepath <- "~/WeeklyData/Raw/KEN_Weekly.xlsx"
#'   df_hfr <- (filepath) }

hfr_import <- function(filepath){

  df <- filepath %>%
    readxl::excel_sheets() %>%
    #setdiff("meta") %>%
    stringr::str_subset("HFR") %>%
    purrr::map_dfr(.f = ~ readxl::read_excel(filepath, sheet = .x, skip = 1, col_types = "text"))

  if("mechanismid" %in% names(df))
    df <- dplyr::rename(df, mech_code = mechanismid)

  return(df)

}
