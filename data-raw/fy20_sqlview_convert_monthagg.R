## PROJECT:  HFR
## AUTHOR:   A.Chafetz | USAID
## AUTHOR:   convert to FY20 to months
## LICENSE:  MIT
## DATE:     2020-11-11
## UPDATED:  2020-11-12

# DEPENDENCIES ------------------------------------------------------------

library(tidyverse)
library(vroom)
library(Wavelength)



# IMPORT DATA -------------------------------------------------------------

folder_path <- "C:/Users/achafetz/Downloads"
df_hfr <- list.files(folder_path, "HFR_2020.01-2020.05_Tableau_20201105|HFR_2020.06-2020.10_Tableau_20201110",full.names = TRUE) %>%
  map_dfr(vroom, col_types = c(.default = "c"))

df_hfr_1113 <- vroom("out/joint/HFR_Tableau_SQLview_ORIGINAL.csv", col_types = c(.default = "c"))


# MUNGE -------------------------------------------------------------------


  #bind sqlview to historic data
    df_hfr <- bind_rows(df_hfr, df_hfr_1113)
    rm(df_hfr_1113)


  #cleaning from R/resolve_sqlview
  df_hfr_clean <- df_hfr %>%
    mutate(primepartner = str_remove(primepartner, "\r$"),
           sex = ifelse(!sex %in% c("Male", "Female"), "Unspecified", sex),
           agecoarse = ifelse(otherdisaggregate %in% c("15+", "<15"), otherdisaggregate, agecoarse),
           agecoarse = ifelse(!agecoarse %in% c("<15", "15+"), "Unknown", agecoarse),
           otherdisaggregate = ifelse(otherdisaggregate == "< 3 months", "<3 months", otherdisaggregate),
           otherdisaggregate = str_replace(otherdisaggregate, "Months", "months"),
           otherdisaggregate = str_replace(otherdisaggregate, "(3|6)m", "\\1 m"),
           otherdisaggregate = ifelse(otherdisaggregate == "6 months", "6 months or more", otherdisaggregate),
           otherdisaggregate = na_if(otherdisaggregate, "0.0"),
           otherdisaggregate = na_if(otherdisaggregate, "nan"),
           otherdisaggregate = na_if(otherdisaggregate, "\\N"),
           otherdisaggregate = na_if(otherdisaggregate, "Positivo"),
           otherdisaggregate = na_if(otherdisaggregate, "Total"),
           otherdisaggregate = na_if(otherdisaggregate, "TGM"),
           otherdisaggregate = na_if(otherdisaggregate, "15+"),
           otherdisaggregate = na_if(otherdisaggregate, "<15"),
           date = ifelse(date == "2109-11-25", "2019-11-25", date),
           fy = ifelse(date == "2020-09-28", "2020", fy),
           hfr_pd = ifelse(date == "2020-09-28", "13", hfr_pd)
           # psnuuid = NA_character_
    ) %>%
    filter(fy != 2110)

  #reshape long to preserve NAs for non reporting
    df_hfr_lng <- df_hfr_clean %>%
      rename(hfr_results = val) %>%
      gather(type, value, hfr_results, mer_results, mer_targets, na.rm = TRUE) %>%
      mutate(value = as.double(value))

    rm(df_hfr,df_hfr_clean)

  #remove date for aggregation
    df_hfr_lng <- select(df_hfr_lng, -date)

  #create a period value for non-TX_CURR indicators
    df_pd_sum <- df_hfr_lng %>%
      filter(!indicator %in% c("TX_CURR", "TX_MMD"),
             type == "hfr_results") %>%
      group_by_if(is.character) %>%
      summarise(value = sum(value, na.rm = TRUE)) %>%
      ungroup()

  #create a period value for TX_CURR indicators
    df_pd_max <- df_hfr_lng %>%
      filter(indicator %in% c("TX_CURR", "TX_MMD") |
               type %in% c("mer_results", "mer_targets")) %>%
      group_by_if(is.character) %>%
      summarise_if(is.numeric, max, na.rm = TRUE) %>%
      ungroup()

  #join period dataset & respread
    df_pds <- bind_rows(df_pd_sum, df_pd_max)  %>%
      spread(type, value)

    rm(df_hfr_lng, df_pd_sum, df_pd_max)


  #generate a list of the first date of the period to apply
    pd_dates <- hfr_identify_pds(2020) %>%
      group_by(hfr_pd) %>%
      filter(date == min(date)) %>%
      ungroup() %>%
      mutate(across(c(hfr_pd, fy), as.character))

  #add on the period date
    df_pds <- df_pds %>%
      left_join(pd_dates) %>%
      relocate(date, .after = hfr_pd)

  #add the period type and expect_reporting to match FY21
    df_pds <- df_pds %>%
      mutate(period_type = "month agg",
             expect_reporting = mer_results > 0 | mer_targets > 0,
             expect_reporting = ifelse(is.na(expect_reporting), FALSE, expect_reporting))

  #rename to align with current SQLView
    df_pds <- df_pds %>%
      rename(val = hfr_results)

  #export
    write_csv(df_pds, "out/fy20_archive/HFR_Tableau_SQLview_FY20.csv", na = "")
    # write_csv(df_pds, "out/joint/HFR_Tableau_SQLview.csv", na = "")



