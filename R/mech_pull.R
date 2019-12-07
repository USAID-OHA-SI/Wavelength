#' Pull Partner/Mechanism Info from DATIM
#'
#' @param usaid_only specify if only USAID mechansism should be returned, default = TRUE
#' @param ou_sel option to specify an operating unit, default = NULL
#' @param folderpath_output provide the full path to the folder for saving
#'
#' @export
#'
#' @examples
#' \dontrun{
#' #pull mechanism/partner information
#' df <- mech_pull() }

mech_pull <- function(fundingagency = TRUE, ou_sel = NULL, folderpath_output = NULL){

  package_check("curl")

  stopifnot(curl::has_internet())

  #url for SQL View stored as a csv
    sql_view_url <- "https://www.datim.org/api/sqlViews/fgUtV6e9YIX/data.csv"

  #pull from DATIM
    df <- readr::read_csv(sql_view_url,
                          col_types = readr::cols(.default = "c"))

  #fitler for OU
    if(!is.null(ou_sel))
      df <- dplyr::filter(df, ou_sel %in% c(ou_sel))

  #filter for USAID mechs if specified
    if(usaid_only == TRUE)
      df <- dplyr::filter(df, fundingagency == "USAID")

  #rename variables to match MSD and remove mech_code from mech_name
    df <- df %>%
      dplyr::select(operatingunit = ou,
                    fundingagency = agency,
                    mech_code = code,
                    primepartner = partner,
                    mech_name = mechanism) %>%
      dplyr::mutate(mech_name = stringr::str_remove(mech_name, "0000[0|1] |[:digit:]+ - "))

  #export
    hfr_export(df, folderpath_output, type = "orghierarchy")

  return(df)
}
