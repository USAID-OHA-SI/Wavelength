#' Aggregate HFR dataframe
#'
#' @param df HFR data frame imported via `hfr_import()`
#'
#' @export

hfr_aggr <-function(df){

  #convert to values numeric and then convert to integers
  df <- dplyr::mutate(df, val = val %>% as.double %>% round %>% as.integer)

  #create month aggregate for weekly data
  df <- hfr_group_wkly(df)

  #identify all grouping vars (everything but val)
  vars_grp <- setdiff(names(df), "val")

  #aggregate (minimize row count)
  df <- df %>%
    dplyr::group_by_at(vars_grp) %>%
    dplyr::summarise_at(dplyr::vars(val), sum, na.rm = TRUE) %>%
    dplyr::ungroup()

  return(df)
}



#' Create Monthly Aggregate for Weekly data
#'
#' @param df HFR data frame imported via `hfr_import()`
#'
#' @export

hfr_group_wkly <- function(df){

  #change frequency from week to month agg and round to month
    df_mo_agg <- df %>%
      dplyr::filter(date > as.Date("2020-09-30"),
                    hfr_freq == "week") %>%
      dplyr::mutate(hfr_freq = "month agg") %>%
      hfr_round_date()

  #bind month agg onto data frame
    df <- dplyr::bind_rows(df, df_mo_agg)

  return(df)

}
