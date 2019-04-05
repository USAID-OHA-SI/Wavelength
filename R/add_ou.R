#' Add Operating Unit Name
#'
#' @param df data frame from high frequency reporting
#' @param operatingunit_name OU name, quoted
#'
#' @export
#'
#' @examples
#' \dontrun{
#' #add missing OU name to reporting dataset
#'   df_uga <- add_ou(df_uga, "Uganda") }

add_ou <- function(df, operatingunit_name){

  df <- df %>%
    dplyr::mutate(operatingunit = operatingunit_name) %>%
    dplyr::select(operatingunit, dplyr::everything())

  return(df)
}
