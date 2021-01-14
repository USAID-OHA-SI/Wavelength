## PROJECT: WAVELENGTH
## AUTHOR:  A.Chafetz | USAID
## PURPOSE: Update DATIM Tables and Upload to S3 Buckets
## LICENSE: MIT
## UPDATED: 2021-01-14


# GLOBAL VARIABLES --------------------------------------------------------

  #load secrets
    glamr::load_secrets()

  #quarters with results in DATIM (Needs to be updated each quarter for updated Results/Targets/Gap Target)
    qtr <- 4

  #DATIM username
    myuser <- glamr::datim_user()

  #folderpath output
    savefolder <- "out/DATIM"


# PULL MER DATA -----------------------------------------------------------

  #identify OU uids to then pull each OU's results/targets
    ous <- identify_levels(username = myuser, password = mypwd(myuser)) %>% dplyr::pull(country_name)

  #pull & export MER site result/targets
    purrr::walk(ous, ~pull_mer(ou_name = .x,
                                    username = myuser,
                                    password = mypwd(myuser),
                                    quarters_complete = qtr,
                                    folderpath_output = savefolder))


# STORE FY21 MER TARGETS (PSNU LEVEL) -------------------------------------

  #latest PSNU MSD (from Panorama)
    df_msd <- list.files("~/Data", "PSNU_IM", full.names = TRUE) %>% readr::read_rds()

  #aggregate FY21 targets for USAID
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

  #rename MSD columns to match HFR
    df_msd_agg <- df_msd_agg %>%
      dplyr::rename(fy = fiscal_year,
                    agecoarse = trendscoarse)
  #export targets
    hfr_export(df_msd_agg, "out/DATIM", type = "DATIM_targets")


# PULL HIERARCHY ----------------------------------------------------------

   #pull hierarchy
    ouuids <- identify_ouuids(myuser, mypwd(myuser)) %>%
      dplyr::filter(is.na(regional)) %>%
      dplyr::pull(id)
    df_orgs <- purrr::map_dfr(.x = ouuids,
                              .f = ~ pull_hierarchy(.x, myuser, mypwd(myuser),
                                                    baseurl = "https://datim.org/"))

  #save hierarchy
    hfr_export(df_orgs, savefolder, type = "orghierarchy")



# PUL MECHANISM INFO ------------------------------------------------------

    pull_mech(folderpath_output = "out/DATIM")


# UPLOAD TABLES TO GOOGLE DRIVE -------------------------------------------

  #org hierarchy table
    gdrive_fldr_orgs <- googledrive::as_id("1enffr2NUZAz5eabUAGmfdXKoKWnLlWZ5")
    gdrive_fldr_orgs_archv <- googledrive::drive_ls(gdrive_fldr_orgs, "Archive") %>% dplyr::pull(id) %>% googledrive::as_id()
    gdrive_fldr_orgs_old <- googledrive::drive_ls(gdrive_fldr_orgs, "HFR_FY21_GLOBAL") %>% dplyr::pull(id) %>% googledrive::as_id()
    googledrive::drive_mv(gdrive_fldr_orgs_old, gdrive_fldr_orgs_archv)

    file <- list.files("out/DATIM", "org", full.names = TRUE) %>% dplyr::last()
    googledrive::drive_upload(file,
                              gdrive_fldr_orgs,
                              name = stringr::str_remove(basename(file), ".csv"),
                              type = "spreadsheet")
  #mech_code table
    gdrive_fldr_mech <- googledrive::as_id("1wMvvO1x8OgVU3aGej4kdAw3F2NbeY-eM")
    gdrive_fldr_mech_archv <- googledrive::drive_ls(gdrive_fldr_mech, "Archive") %>% dplyr::pull(id) %>% googledrive::as_id()
    gdrive_fldr_mech_old <- googledrive::drive_ls(gdrive_fldr_mech, "HFR_FY21_GLOBAL") %>% dplyr::pull(id) %>% googledrive::as_id()
    googledrive::drive_mv(gdrive_fldr_mech_old, gdrive_fldr_mech_archv)

    file <- list.files("out/DATIM", "mechanisms", full.names = TRUE) %>% dplyr::last()
    googledrive::drive_upload(file,
                              gdrive_fldr_mech,
                              name = stringr::str_remove(basename(file), ".csv"),
                              type = "spreadsheet")


# APPEND TABLEAU FILE -----------------------------------------------------

  #FY21 HFR DDC files (outputs from Trifecta)
    ddc_path <- list.files("C:/Users/achafetz/Downloads",
                           glue::glue("hfr_2021.*{Sys.Date()}"), full.names = TRUE)

  #historic FY20 HFR data path
    fy20_path <- "out/fy20_archive/HFR_Tableau_SQLview_FY20.zip"

  #read in DDC files (append together)
    df_ddc <- vroom::vroom(ddc_path, col_types = c(.default = "c"))

  #read in FY20 HFR
    df_fy20 <- vroom::vroom(fy20_path, col_types = c(.default = "c"))

  #append FY20 + FY21
    twbx <- dplyr::bind_rows(df_fy20, df_ddc)

  #write output to file need for Tableau
    readr::write_csv(twbx, "out/joint/HFR_Tableau_SQLview.csv", na = "")


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

