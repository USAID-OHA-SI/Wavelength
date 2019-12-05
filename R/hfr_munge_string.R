#' clean up/standardize string text for indicators and disaggs
#'
#' @param df HFR data frame imported via `hfr_import()`
#'
#' @export
#'

hfr_munge_string <- function(df){

  df <- df %>%
    dplyr::mutate(indicator = toupper(indicator),
                  indicator = ifelse(indicator == "PREP_NEW", "PrEP_NEW", indicator),
                  agecoarse = dplyr::recode(agecoarse, "u15" = "<15", "o15" = "15+"),
                  sex = dplyr::recode(sex,
                                      "f" = "Female",
                                      "F" = "Female",
                                      "m" = "Male",
                                      "M" = "Male"),
                  otherdisaggregate = dplyr::recode(otherdisaggregate,
                                                    "u3mo" = "<3 months",
                                                    "35mo" = "3-5 months",
                                                    "o6mo" = "6 months or more"
                                                    #.default = as.character(NA)
                                                    ),
                  mech_code = stringr::str_remove(mech_code, "\\.0")
                  )
  return(df)
}
