#' Validation on import
#'
#' @param df df create during `hfr_import()`
#'
#' @export

validate_import <- function(df){

  #check columns
  if(var_exists(df, "val")){
    req_cols <- template_cols_long
  } else if(var_exists(df, "hts_tst.NA.NA")){
    req_cols <- template_cols_wide_lim
  } else {
    req_cols <- template_cols_wide
  }

  missing <- flag_missing(req_cols, names(df))
  extra <- flag_extra(req_cols, names(df))

  #PRINT VALIDATION

  cat("\nAre there any missing columns on import?", missing,
      "\nAre there any extra columns on import?", extra)

  check_distinct_ous(df)

  check_frequency(df)
}


#' Check OUs listed in operatingunit
#'
#' @param df df create during `hfr_import()`
#'
#' @export

check_distinct_ous <- function(df){

  ous <- df %>%
    dplyr::distinct(operatingunit) %>%
    dplyr::pull(operatingunit) %>%
    paste(collapse = ", ")

  multi_ous <- length(ous) > 1

  ou_out <- ifelse(multi_ous == TRUE, crayon::yellow(ous), crayon::blue(ous))

  #print validation
  cat("\nIs there just one OU (for non regional OUs)?", ou_out)
}


#' Check OUs listed in operatingunit
#'
#' @param df df create during `hfr_import()`
#'
#' @export

check_frequency <- function(df){

  # Identify frequency(ies)
  freqs <- df %>%
    dplyr::count(
      orgunituid,
      mech_code,
      indicator,
      sex,
      agecoarse,
      otherdisaggregate) %>%
    dplyr::distinct(n) %>%
    dplyr::pull(n)

  # Flag multiple frequencies
  multi_freqs <- length(freqs) > 1

  # Labels
  pd_freq <- dplyr::if_else(
    multi_freqs == FALSE & freqs[1] == 1,
    "Monthly data",
    dplyr::if_else(multi_freqs == FALSE & freqs[1] == 4,
                   "Weekly data",
                   dplyr::if_else(multi_freqs == FALSE & freqs[1] == 5,
                                  "Monthly along with weekly data",
                                  "Mixed frequencies")))

  # Sub-labels
  pd_freqs <- dplyr::if_else(
    multi_freqs == TRUE & dplyr::setequal(c(1, 4), freqs),
    "Monthly and Weekly data",
    dplyr::if_else(multi_freqs == TRUE & dplyr::setequal(c(1, 5), freqs),
            "Monthly and/or Weekly",
            dplyr::if_else(multi_freqs == TRUE & dplyr::setequal(c(1, 4, 5), freqs),
                    "Weekly, Monthly, and Monthly along with weekly",
                    "Unknown frequency ...")))

  flag_freqs <- dplyr::if_else(
    multi_freqs == FALSE,
    crayon::blue(pd_freq),
    crayon::blue(paste0(pd_freq, ": ", pd_freqs))
  )

  #print validation
  cat("\nWhat are the frequencies being submitted?", flag_freqs)
}
