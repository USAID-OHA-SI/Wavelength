#' Validation Checks
#'
#' @param df HFR data framed created by `process_template()``
#'
#' @export

run_checks <- function(df){

  #check headers
  check_names <- all(names(df) %in% c("date", "fy", "hfr_pd", "orgunit",
                                      "orgunituid", "mech_code", "partner",
                                      "operatingunit", "psnu", "indicator",
                                      "agecoarse", "sex", "otherdisaggregate", "val"))


  #check dates
  check_date <- dplyr::count(df, date, hfr_pd)

  #check orgunituids
  check_orgunituid <- dplyr::count(df, is.na(orgunituid))

  #check mech_code
  check_mechcode <- dplyr::count(df, mech_code)

  #check operatingunit
  check_operatingunit <- dplyr::distinct(df, operatingunit)

  #check indicator
  check_ind <- df %>%
    dplyr::distinct(indicator) %>%
    dplyr::pull() %in% c("HTS_TST", "HTS_TST_POS", "PrEP_NEW",
                         "TX_CURR", "TX_MMD", "TX_NEW", "VMMC_CIRC")
  #check agecoarse
  check_age <- dplyr::distinct(df, agecoarse)

  #check sex
  check_sex <- dplyr::distinct(df, sex)

  #check months
  check_months <- dplyr::distinct(df, otherdisaggregate)

  return(list(check_names, check_date, check_orgunituid, check_mechcode,
              check_operatingunit, check_ind, check_age, check_sex, check_months))
}
