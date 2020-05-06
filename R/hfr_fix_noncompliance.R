#' Resolve issues with non-standard entries
#'
#' @param df HFR data frame imported via `hfr_import()`
#'
#' @export

hfr_fix_noncompliance <- function(df){
  df <- df %>%
    #resolve issue with reporting
    dplyr::mutate(
                  #CDI
                  indicator = ifelse(indicator %in% c("TX_NEW_NewHIV", "TX_NEW_NEWHIV"), "TX_NEW", indicator),
                  #Laos
                  indicator = stringr::str_replace_all(indicator, "-", "_"),
                  agecoarse = ifelse(agecoarse == "Male 20-25", "15+", agecoarse),
                  #Lesotho
                  indicator = ifelse(indicator == "HTS_POS", "HTS_TST_POS", indicator),
                  #Nigeria
                  otherdisaggregate = dplyr::case_when(otherdisaggregate %in% c("2Months","< 3 months", "<3months", "<3 Months")  ~ "<3 months",
                                                       otherdisaggregate %in% c("3Months", "4Months", "5Months", "3-5 month","3-5Months") ~ "3-5 months",
                                                       otherdisaggregate == "6Months" ~ "6 months or more",
                                                       TRUE ~ otherdisaggregate),
                  #Moz
                  otherdisaggregate = dplyr::case_when(indicator == "TX_3MD" ~ "3-5 months",
                                                       indicator == "< 3 months" ~ "<3 months",
                                                       TRUE ~ otherdisaggregate),
                  indicator = ifelse(indicator == "TX_3MD", "TX_MMD", indicator),
                  #South Africa
                  indicator = dplyr::case_when(indicator == "TX_CURR_90" ~ "TX_CURR",
                                               indicator == "MULTI DRUG" ~ "TX_MMD",
                                               TRUE ~ indicator),
                  otherdisaggregate = dplyr::case_when(otherdisaggregate %in% c("1", "2", "1 Month", "2 Months") ~ "<3 months",
                                                       otherdisaggregate %in% c("3", "4", "5", "3 Months", "4 Months", "5 Months") ~ "3-5 months",
                                                       otherdisaggregate %in% c("6", "6+", "6+ months", "6+ Months","6 Months") ~ "6 months or more",
                                                       TRUE ~ otherdisaggregate),
                  #South Sudan
                  val = dplyr::na_if(val, "N/A"),
                  val = dplyr::na_if(val, "M/A"),
                  #Thailand
                  indicator = dplyr::case_when(indicator == "HTS_TST ALL" ~ "HTS_TST",
                                               indicator == "HTS_TST.POS" ~ "HTS_TST",
                                               indicator == "HTS_TST POSITIVE" ~ "HTS_TST_POS",
                                               TRUE ~ indicator),
                  agecoarse = as.character(agecoarse),
                  agecoarse = dplyr::case_when(agecoarse %in% c("15-19", "20-24", "30-34",
                                                         "25-29", "35-39", "40-44", "45-49",
                                                         "40-49", "50+") ~ "15+",
                                        TRUE ~ agecoarse),
                  otherdisaggregate = dplyr::case_when(otherdisaggregate %in% c("MSM", "MSW", "TG",
                                                                         "TG-SW", "TGSW", "Female Non-KP",
                                                                         "FSW", "Male Non-KP") ~ as.character(NA),
                                                TRUE ~ otherdisaggregate),

                  #Vietnam
                  indicator = stringr::str_replace(indicator, "MMS", "MMD"),
                  operatingunit = ifelse(operatingunit == "Viet Nam", "Vietnam", operatingunit)
                  ) %>%
    dplyr::mutate_at(dplyr::vars(agecoarse, sex), ~ dplyr::na_if(., "NA"))


  return(df)
}

