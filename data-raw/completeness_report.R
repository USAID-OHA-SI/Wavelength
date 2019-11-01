## PROJECT:  HFR
## AUTHOR:   A.CHAFETZ | USAID
## PURPOSE:  OU completeness report
## DATE:     2019-10-03
## UPDATED:  2019-10-14


# DEPENDENCIES ------------------------------------------------------------

  library(magrittr)

# VARIABLES ---------------------------------------------------------------

  #global file path
    path <- "out/joint/HFR_GLOBAL_output_PD1220191004.1513.csv"

  #numeric variables to convert from string
    vars_num <- c("mer_results", "mer_targets", "targets_gap", "weekly_targets", "weekly_targets_gap", "val")

  #list of OUs to review
    ou_lst <- c("Botswana", "Burundi", "Cameroon", "Cote d'Ivoire",
                "Democratic Republic of the Congo", "Dominican Republic",
                "Eswatini", "Ethiopia", "Haiti", "Kenya", "Lesotho",
                "Malawi", "Mozambique", "Namibia", "Nigeria", "South Africa",
                "South Sudan", "Tanzania", "Uganda", "Ukraine", "Vietnam",
                "Zambia", "Zimbabwe")

# IMPORT DATASET ----------------------------------------------------------

  #import, reading in all as character as default
    df_glob <- readr::read_csv(path, col_types = c(.default = "c"))


# MUNGE -------------------------------------------------------------------

  #adjust numeric variables and fix issue with lines w/ HTS_TST POS
    df_glob <- df_glob %>%
      dplyr::mutate_at(dplyr::vars(vars_num), as.numeric) %>%
      dplyr::mutate(indicator = ifelse(indicator == "HTS_TST POS", "HTS_TST_POS", indicator))

  #filter out PD 8 (not officially collected) & restrict to OU's in list; shorten to DRC
    df_glob <-  df_glob %>%
      dplyr::filter(hfr_pd != "8",
                    operatingunit %in% ou_lst) %>%
      dplyr::mutate(operatingunit = ifelse(operatingunit == "Democratic Republic of the Congo", "DRC", operatingunit))

  #create denom for TX_MMD
    df_glob_mmd <- df_glob %>%
      dplyr::filter(indicator == "TX_CURR") %>%
      dplyr::group_by(operatingunit, snu1, psnu, orgunit, hfr_pd, indicator) %>% #orgunituid
      dplyr::summarise_at(dplyr::vars(mer_targets), sum, na.rm = TRUE) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(indicator = "TX_MMD")


# SUMMARIZE COMPLETENESS --------------------------------------------------

  #append MMD "target" sites
    df_glob_sites <- dplyr::bind_rows(df_glob, df_glob_mmd)

  #distinct set of sites w/ their MER targets and HFR values
    df_glob_sites <- df_glob_sites %>%
      dplyr::group_by(operatingunit, snu1, psnu, orgunit, hfr_pd, indicator) %>% #orgunituid
      dplyr::summarise_at(dplyr::vars(mer_targets, val), sum, na.rm = TRUE) %>%
      dplyr::ungroup()

  #create site count for targets & HFR reporting
    df_glob_sites_comp <- df_glob_sites %>%
      dplyr::mutate(target_sitecnt = ifelse(mer_targets > 0, 1, 0),
                    hfr_sitecnt = ifelse(val > 0, 1, 0)) %>%
      dplyr::group_by(operatingunit, indicator, hfr_pd) %>%
      dplyr::summarise_at(dplyr::vars(mer_targets, val, target_sitecnt, hfr_sitecnt), sum, na.rm = TRUE) %>%
      dplyr::ungroup()

  #record the distinct site count for each OUxIndicator
    df_glob_sites_n <- df_glob_sites_comp %>%
      dplyr::group_by(operatingunit, indicator) %>%
      dplyr::summarise_at(dplyr::vars(target_sitecnt), max, na.rm = TRUE) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(indicator = paste0(indicator, "_n")) %>%
      dplyr::rename(completeness = target_sitecnt)

  #create completeness metric
    df_glob_sites_comp <- df_glob_sites_comp %>%
      dplyr::mutate(completeness = hfr_sitecnt/target_sitecnt)

  #clean for export to Excel (clipr - value to clipboard)
    df_glob_sites_comp %>%
      dplyr::select(-c(mer_targets, val, target_sitecnt, hfr_sitecnt)) %>%
      dplyr::mutate(hfr_pd = stringr::str_pad(hfr_pd, 2, pad = "0")) %>%
      dplyr::arrange(operatingunit, indicator, hfr_pd) %>%
      tidyr::unite("indicator", c("indicator", "hfr_pd"), sep = "_pd") %>%
      dplyr::bind_rows(df_glob_sites_n) %>%
      tidyr::spread(indicator, completeness) %>%
      dplyr::mutate_at(dplyr::vars(dplyr::ends_with("_n")), ~ dplyr::na_if(., 0)) %>%
      dplyr::select(operatingunit, dplyr::starts_with("HTS_TST"), dplyr::starts_with("TX_NEW"),
                    dplyr::starts_with("VMMC_CIRC"), dplyr::starts_with("PrEP_NEW"),
                    dplyr::starts_with("TX_CURR"), dplyr::starts_with("TX_MMD"))

