#' Pull Partner/Mechanism Info from DATIM
#'
#' @export
#'
#' @examples
#' \dontrun{
#' #pull mechanism/partner information
#' df <- mech_pull() }

mech_pull <- function(){

  package_check("curl")

  stopifnot(curl::has_internet())

  #url for SQL View stored as a csv
    sql_view_url <- "https://www.datim.org/api/sqlViews/fgUtV6e9YIX/data.csv"

  #pull from DATIM
    df <- readr::read_csv(sql_view_url,
                          col_types = readr::cols(.default = "c"))

  #rename variables to match MSD and remove mech_code from mech_name
  df <- df %>%
    dplyr::select(mech_code = code,
                  primepartner = partner,
                  mech_name = mechanism) %>%
    dplyr::mutate(mech_name = stringr::str_remove(mech_name_d, "0000[0|1] |[:digit:]+ - "))

  return(df)
}
