#' Aggregate HFR dataframe
#'
#' @param df HFR data frame imported via `hfr_import()`
#'
#' @export

hfr_aggr <-function(df){

  #convert to values to double
  df <- dplyr::mutate(df, val = as.double(val))

  #identify all grouping vars (everything but val)
  vars_grp <- setdiff(names(df), "val")

  #aggregate (minimize row count)
  df <- df %>%
    dplyr::group_by_at(vars_grp) %>%
    dplyr::summarise_at(dplyr::vars(val), sum, na.rm = TRUE) %>%
    dplyr::ungroup()

  return(df)
}
