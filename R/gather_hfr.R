#' Reshape HFR Data frame long
#'
#' @param df HFR data frame imported via `import_hfr()`
#'
#' @export

gather_hfr <- function(df){

  #only need to gather if the data set is wide (will not have indicator as column)

    if(!var_exists(df, "val")){

      #identify data columns
        data_cols <- setdiff(names(df), template_cols_meta)

      #reshape from wide to long
        df <- tidyr::gather(df, ind, val, data_cols)

      #seperate former col names into indicator & disaggregates
        df <- tidyr::separate(df, ind, c("indicator", "agecoarse", "sex", "otherdisaggregate"),
                              sep = "\\.", fill = "right")
    }

  return(df)

}

