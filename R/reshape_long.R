#' Reshape data long
#'
#' @param df wide data frame to reshape
#' @param meta_group meta data, no values, eg PSNU, partner, indicator
#'
#' @export

reshape_long <- function(df, meta_group){

  df <- df %>%
    dplyr::mutate_all(~ dplyr::na_if(., 0)) %>%
    tidyr::gather(ind, val, -dplyr::one_of(meta_group), na.rm = TRUE) %>%
    dplyr::mutate(val = as.numeric(val))

  return(df)
}
