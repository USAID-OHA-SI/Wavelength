# Pull from DATIM ---------------------------------------------------------

  ## NOTE: Needs to be updated each quarter for updated Results/Targets/Gap Target

  #DATIM username
    myuser <- ""

  #quarters with results in DATIM
    qtr <- 3

  #folderpath output
    savefolder <- "out/DATIM"

  #pull MER data
    ous <- identify_levels(username = myuser, password = mypwd(myuser)) %>% dplyr::pull(country_name)
    purrr::walk(ous, ~pull_mer(ou_name = .x,
                                    username = myuser,
                                    password = mypwd(myuser),
                                    quarters_complete = qtr,
                                    folderpath_output = savefolder))

  #FY21 MER PSNU data from MSD
    df_msd <- list.files("~/Data", "PSNU_IM", full.names = TRUE) %>% readr::read_rds()

    df_msd_agg <- df_msd %>%
      dplyr::filter(fiscal_year == 2021,
                    fundingagency == "USAID",
                    indicator %in% c("HTS_TST", "HTS_TST_POS", "TX_NEW", "TX_CURR", "PrEP_NEW", "VMMC_CIRC"),
                    standardizeddisaggregate %in% c("Modality/Age/Sex/Result", "Modality/Age Aggregated/Sex/Result","Age/Sex", "Age Aggregated/Sex/HIVStatus" ,"Age/Sex/HIVStatus"),
                    !(indicator == "VMMC_CIRC" & standardizeddisaggregate == "Age/Sex")) %>%
      dplyr::group_by(psnu, psnuuid,
                      fiscal_year, fundingagency, mech_code,	mech_name,
                      indicator, sex,	trendscoarse,
                      operatingunit, countryname, snu1) %>%
      dplyr::summarise(mer_targets = sum(targets, na.rm = TRUE)) %>%
      dplyr::ungroup()

    df_msd_agg <- df_msd_agg %>%
      dplyr::rename(fy = fiscal_year,
                    agecoarse = trendscoarse)


    hfr_export(df_msd_agg, "out/DATIM", type = "DATIM_targets")

   #pull hierarchy
    ouuids <- identify_ouuids(myuser, mypwd(myuser)) %>%
      dplyr::filter(is.na(regional)) %>%
      dplyr::pull(id)
    df_orgs <- purrr::map_dfr(.x = ouuids,
                              .f = ~ pull_hierarchy(.x, myuser, mypwd(myuser)))

    hfr_export(df_orgs, savefolder, type = "orghierarchy")



  #pull mechanism info
    pull_mech(folderpath_output = "out/DATIM")

  #upload to Google Drive
    googledrive::drive_auth()

    file <- list.files("out/DATIM", "org", full.names = TRUE) %>% dplyr::last()
    googledrive::drive_upload(file,
                              googledrive::as_id("1enffr2NUZAz5eabUAGmfdXKoKWnLlWZ5"),
                              name = stringr::str_remove(basename(file), ".csv"),
                              type = "spreadsheet")

    file <- list.files("out/DATIM", "mechanisms", full.names = TRUE) %>% dplyr::last()
    googledrive::drive_upload(file,
                              googledrive::as_id("1wMvvO1x8OgVU3aGej4kdAw3F2NbeY-eM"),
                              name = stringr::str_remove(basename(file), ".csv"),
                              type = "spreadsheet")

# Process HFR submissions -------------------------------------------------

  #hfr submission period
    submission_pd <- 5

  #processed/output folder
    savefolder <- "out/processed"

  #store list of HFR submissions
    (files <- list.files("ou_submissions", full.names = TRUE))

  #validate submissions
    purrr::walk(files, hfr_process_template)

  #save processed files if submissions meet validation checks(filter for just submission pd)
    purrr::walk(files, hfr_process_template, round_hfrdate = TRUE,
                 hfr_pd_sel = submission_pd, folderpath_output = savefolder)

