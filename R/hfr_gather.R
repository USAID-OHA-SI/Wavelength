#' Reshape HFR Data frame long
#'
#' @param df HFR data frame imported via `hfr_import()`
#'
#' @export

hfr_gather <- function(df){

  #only need to gather if the data set is wide (will not have indicator as column)

    if(!var_exists(df, "val")){

      #identify data columns
        data_cols <- setdiff(names(df), template_cols_meta)

      #reshape from wide to long
        df <- tidyr::gather(df, ind, val, all_of(data_cols), na.rm = TRUE)

      #seperate former col names into indicator & disaggregates
        df <- tidyr::separate(df, ind, c("indicator", "agecoarse", "sex", "otherdisaggregate"),
                              sep = "\\.", fill = "right")
      #reorganize
        df <- dplyr::select(df, date:indicator, sex, dplyr::everything())
    }

  return(df)

}

