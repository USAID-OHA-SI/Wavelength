## PROJECT: Hight Frequency Reporting
## AUTHOR:  Aaron Chafetz | USAID
## PURPOSE: Site completeness for Jhpiego 2020.01-2020.03
## DATE:    2020-01-27
## UPDATED:


# Dependencies ------------------------------------------------------------

library(tidyverse)

# Idenitfy JHPIEGO mechs with 2020 targets --------------------------------

  #read in MSD
    df_msd <- list.files("~/Data", "OU_IM", full.names = TRUE) %>%
      read_rds()

  #identify which mechanism fall under Jhpiego
    df_msd_jhpiego <- df_msd %>%
      filter(str_detect(primepartner, "JHP|jhp"),
             fundingagency == "USAID",
             indicator %in% c("HTS_TST", "TX_NEW", "TX_CURR", "VMMC_CIRC", "PrEP_NEW"),
             standardizeddisaggregate == "Total Numerator",
             fiscal_year == 2020) %>%
      count(fiscal_year, operatingunit, mech_code, primepartner, indicator, wt = targets) %>%
      spread(indicator, n)

  #pull Jhpiego mech list
    lst_mechs <- unique(df_msd_jhpiego$mech_code)


# Extract Sites for JHPIEGO mechanisms ------------------------------------

  #import site targets from DATIM extracts
    df_datim <- map_dfr(list.files("out/DATIM", full.names = TRUE), read_csv)

    df_datim <- mutate(df_datim, mech_code = as.character(mech_code))

  #filter to Jhpiego & aggregate
    df_jhp_sites <- df_datim %>%
      filter(mech_code %in% lst_mechs) %>%
      group_by(operatingunit, orgunit, orgunituid, mech_code, mech_name, indicator) %>%
      summarise_at(vars(mer_targets), sum, na.rm = TRUE) %>%
      ungroup()

  #repeat site targets for each HFR period to get full list of reporting
    pds <- seq(1, length.out =3)
    df_jhp_sites_rpt <- map_dfr(.x = pds,
                                .f = ~mutate(df_jhp_sites, hfr_pd = .x)) %>%
                        mutate(fy = 2020)


  #add MMD "targets" from TX_CURR
    df_jhp_sites_rpt <- df_jhp_sites_rpt %>%
      filter(indicator == "TX_CURR") %>%
      mutate(indicator = "TX_MMD") %>%
      bind_rows(df_jhp_sites_rpt, .)


# Compare to HFR ----------------------------------------------------------

  #identify all Jhpiego submissions & import
    files <- list.files("ou_submissions/Jhpiego", full.names = TRUE)

    df_hfr_jph <- map_dfr(files, hfr_process_template, round_hfrdate = TRUE)

  #adjust names for merging purposes (keep and replace when missing in MER)
    df_hfr_jph <- df_hfr_jph %>%
      rename_at(vars(operatingunit, partner, orgunit), ~ paste0(., "_hfr"))

  #aggregate non TX_CURR/MMD HFR values
    df_hfr_jph_agg <- df_hfr_jph %>%
      filter(!indicator %in% c("TX_CURR","TX_MMD")) %>%
      group_by(fy, hfr_pd, operatingunit_hfr, orgunit_hfr, orgunituid, mech_code, partner_hfr, indicator) %>%
      summarise(hfr_val = sum(val, na.rm = TRUE)) %>%
      ungroup()

  #aggregate TX_CURR/MMD (max b/c reporting should be only once per pd and are snapshot indicators)
    df_hfr_jph_agg_nn <- df_hfr_jph %>%
      filter(indicator %in% c("TX_CURR","TX_MMD")) %>%
      group_by(fy, hfr_pd, date, operatingunit_hfr, orgunit_hfr, orgunituid, mech_code, partner_hfr, indicator) %>%
      summarise(hfr_val = sum(val, na.rm = TRUE)) %>%
      ungroup() %>%
      group_by(fy, hfr_pd, operatingunit_hfr, orgunit_hfr, orgunituid, mech_code, partner_hfr, indicator) %>%
      summarise(hfr_val = max(hfr_val, na.rm = TRUE)) %>%
      ungroup()

  #join HFR data together
    df_hfr_jph_agg <- bind_rows(df_hfr_jph_agg, df_hfr_jph_agg_nn)
      rm(df_hfr_jph_agg_nn)


# Completeness ------------------------------------------------------------

  #full join of HFR and DATIM data (replacing missing vars if not in MER)
    df_joint <- full_join(df_jhp_sites_rpt, df_hfr_jph_agg) %>%
      mutate(operatingunit = ifelse(is.na(operatingunit), operatingunit_hfr, operatingunit),
                    orgunit = ifelse(is.na(orgunit), orgunit_hfr, orgunit),
                    mech_name = ifelse(is.na(mech_name), partner_hfr, mech_name)) %>%
      select(-ends_with("_hfr")) %>%
      select(-mer_targets, -hfr_val, everything()) %>%
      arrange(operatingunit, mech_code, orgunit, indicator, hfr_pd)

  #identify reporting types
    df_joint <- df_joint %>%
      mutate(type = case_when(!is.na(mer_targets) & is.na(hfr_val) ~ "Sites with no HFR reported",
                                            is.na(mer_targets) & !is.na(hfr_val) ~ "Sites where HFR reported where no FY20 targets",
                                            TRUE ~ "Sites where HFR reported against FY20 targets"))

  #export sites and their reporting/targets
    write_csv(df_joint, "out/HFR_FY20_Jhpiego_sites.csv", na = "")

  #export site count by
    df_joint_count <- df_joint %>%
      count(operatingunit, mech_code, indicator, fy, hfr_pd, type) %>%
      spread(type, n) %>%
      mutate_if(is.integer, ~ ifelse(is.na(.), 0, .)) %>%
      mutate(`Site Reporting Completeness` = `Sites where HFR reported against FY20 targets`/(`Sites where HFR reported against FY20 targets` +`Sites with no HFR reported`),
                    `Site Reporting Completeness` = ifelse(is.nan(`Site Reporting Completeness`), NA, `Site Reporting Completeness`)) %>%
      mutate_at(vars(`Sites where HFR reported against FY20 targets`, `Sites with no HFR reported`, `Sites where HFR reported where no FY20 targets`), ~ na_if(., 0)) %>%
      select(-`Sites where HFR reported where no FY20 targets`, -`Site Reporting Completeness`, everything())

  #export table
    write_csv(df_joint_count, "out/HFR_FY20_Jhpiego_completeness.csv", na = "")
