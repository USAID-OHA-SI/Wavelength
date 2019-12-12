#' Resolve issues with non-standard entries
#'
#' @param df HFR data frame imported via `hfr_import()`
#'
#' @export

hfr_fix_noncompliance <- function(df){
  df <- df %>%
    #resolve issue with reporting for Thailand
    dplyr::mutate(indicator = dplyr::case_when(indicator == "HTS_TST ALL" ~ "HTS_TST",
                                               indicator == "HTS_TST POSITIVE" ~ "HTS_TST_POS",
                                               TRUE ~ indicator),
                  agecoarse = dplyr::case_when(agecoarse %in% c("15-19", "20-24", "30-34",
                                                         "25-29", "35-39", "40-44", "45-49",
                                                         "50+") ~ "15+",
                                        TRUE ~ agecoarse),
                  otherdisaggregate = dplyr::case_when(otherdisaggregate %in% c("MSM", "MSW", "TG",
                                                                         "TG-SW", "Female Non-KP",
                                                                         "FSW", "Male Non-KP") ~ as.character(NA),
                                                TRUE ~ otherdisaggregate),
                  #Laos
                  indicator = stringr::str_replace_all(indicator, "-", "_"),
                  #Lesotho
                  indicator = ifelse(indicator == "HTS_POS", "HTS_TST_POS", indicator),
                  #Vietnam
                  indicator = stringr::str_replace(indicator, "MMS", "MMD"),
                  #South Sudan
                  val = dplyr::na_if(val, "N/A"),
                  val = dplyr::na_if(val, "M/A"),
                  #CDI
                  indicator = ifelse(indicator %in% c("TX_NEW_NewHIV", "TX_NEW_NEWHIV"), "TX_NEW", indicator),
                  #Moz
                  otherdisaggregate = ifelse(indicator == "TX_3MD", "3-5 months", otherdisaggregate),
                  indicator = ifelse(indicator == "TX_3MD", "TX_MMD", indicator)

                  )


  return(df)
}

