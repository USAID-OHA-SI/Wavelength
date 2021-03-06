## PROJECT: WAVELENGTH
## AUTHOR:  A.Chafetz, B.Kagniniwa, T.Essam | USAID
## PURPOSE: Update DATIM Tables and Upload to S3 Buckets
## LICENSE: MIT
## UPDATED: 2021-03-11

# LIBRARIES ---------------------------------------------------------------

library(tidyverse)
library(Wavelength)
library(glamr)
library(fs)

# GLOBAL VARIABLES --------------------------------------------------------

  #load secrets
    glamr::load_secrets()

  #quarters with results in DATIM (Needs to be updated each quarter for updated Results/Targets/Gap Target)
    qtr <- 1

  #DATIM username
    myuser <- glamr::datim_user()

  #folderpath output
    savefolder <- "out/DATIM"

  #DDC Buckets
    hfr_bucket <- "gov-usaid"


# PULL MER DATA -----------------------------------------------------------

  #identify OU uids to then pull each OU's results/targets
    ous <- Wavelength::identify_levels(
      username = myuser,
      password = glamr::datim_pwd()
    ) %>%
    dplyr::pull(country_name)

  #pull & export MER site result/targets
    # purrr::walk(ous, ~pull_mer(ou_name = .x,
    #                            username = myuser,
    #                            password = glamr::datim_pwd(),
    #                            quarters_complete = qtr,
    #                            folderpath_output = savefolder))

    df_mer <- purrr::map_dfr(ous, ~pull_mer(ou_name = .x,
                                            username = myuser,
                                            password = glamr::datim_pwd()))

    hfr_export(df_mer, "out/DATIM", type = "DATIM", quarters_complete = qtr)


# API VS MSD COMPARISON ---------------------------------------------------

    # df_msd_agg <- si_path() %>%
    #   return_latest("OU_IM") %>%
    #   read_rds()
    #
    # df_msd_agg <- df_msd_agg %>%
    #   dplyr::filter(fiscal_year == 2021,
    #                 fundingagency == "USAID",
    #                 indicator %in% c("HTS_TST", "HTS_TST_POS", "TX_NEW", "TX_CURR", "PrEP_NEW", "VMMC_CIRC"),
    #                 standardizeddisaggregate == "Total Numerator") %>%
    #   dplyr::group_by(countryname, indicator) %>%
    #   dplyr::summarise(mer_results = sum(cumulative, na.rm = TRUE),
    #                    mer_targets = sum(targets, na.rm = TRUE)) %>%
    #   dplyr::ungroup() %>%
    #   tidyr::pivot_longer(starts_with("mer"), names_to = "type", values_to = "value_msd")
    #
    # df_api_agg <- df_mer %>%
    #   dplyr::filter(indicator %in% c("HTS_TST", "HTS_TST_POS", "TX_NEW", "TX_CURR", "PrEP_NEW", "VMMC_CIRC")) %>%
    #   dplyr::group_by(countryname, indicator) %>%
    #   dplyr::summarise(mer_results = sum(mer_results, na.rm = TRUE),
    #                    mer_targets = sum(mer_targets, na.rm = TRUE)) %>%
    #   dplyr::ungroup() %>%
    #   tidyr::pivot_longer(starts_with("mer"), names_to = "type", values_to = "value_api")
    #
    # df_combo <- full_join(df_msd_agg, df_api_agg)
    #
    # df_combo %>%
    #   dplyr::mutate(value_api = ifelse(is.na(value_api), 0, value_api),
    #                 variance = value_api/value_msd,
    #                 variance = ifelse(is.nan(variance), 1, variance)) %>%
    #   dplyr::arrange(variance) %>%
    #   glamr::prinf()

# STORE FY21 MER TARGETS (PSNU LEVEL) -------------------------------------

  #latest PSNU MSD (from Panorama)
    df_msd <- si_path() %>%
      return_latest("OU_IM") %>%
      read_rds()

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
    ouuids <- Wavelength::identify_ouuids(myuser, glamr::datim_pwd()) %>%
      dplyr::filter(is.na(regional)) %>%
      dplyr::pull(id)

    df_orgs <- purrr::map_dfr(.x = ouuids,
                              .f = ~ pull_hierarchy(.x, myuser,
                                                    glamr::datim_pwd(),
                                                    baseurl = "https://datim.org/"))

  #save hierarchy
    hfr_export(df_orgs, savefolder, type = "orghierarchy")



# PULL MECHANISM INFO ------------------------------------------------------

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



# UPLOAD REFERENCE TABLES to S3 -------------------------------------------

  # Mechanisms
  list.files(
      path = savefolder,
      pattern = "^HFR_FY\\d{2}_GLOBAL_mechanisms_\\d{8}.csv$",
      full.names = TRUE
    ) %>%
    sort() %>%
    last() %>%
    glamr::s3_upload(
      file = .,
      bucket = hfr_bucket,
      prefix = "ddc/uat/raw/hfr/receiving"
    )

  # Org hierarchy
  list.files(
      path = savefolder,
      pattern = "^HFR_FY\\d{2}_GLOBAL_orghierarchy_\\d{8}.csv$",
      full.names = TRUE
    ) %>%
    sort() %>%
    last() %>%
    glamr::s3_upload(
      file = .,
      bucket = hfr_bucket,
      prefix = "ddc/uat/raw/hfr/receiving"
    )



# ADD NEW SUBMISSIONS TO S3 -----------------------------------------------

# DOWNLOAD SUBMISSIONS FROM GOOGLE DRIVE
# Note: Make sure to update "Send to DDC for processing" column

  #identify all files in Google Drive directory to compare
    gdrive_submissions <- googledrive::as_id("0B9c20El0HKU1fldGdnQ3enBfQlFhSW9rQ21XUzdKT2tLdkNRSFlWNnladllLOGY3em5OdWs")

    df_submissions <- googledrive::drive_ls(gdrive_submissions) %>%
      dplyr::select(name, id) %>%
      dplyr::mutate(exists_gdrive = TRUE)

  #identify all files in s3 bucket to compare
    df_incoming <- glamr::s3_objects(
      bucket = 'gov-usaid',
      prefix = "ddc/uat/raw/hfr/incoming/",
      n = Inf) %>%
      glamr::s3_unpack_keys() %>%
      dplyr::filter(nchar(sys_data_object) > 1,
                    # last_modified <= as.Date("2021-02-16")
                    ) %>%
      dplyr::select(sys_data_object) %>%
      dplyr::mutate(exist_s3 = TRUE)

  #filter for files needed to add to s3 bucket
    df_missing <- df_submissions %>%
      dplyr::full_join(df_incoming, by = c("name" = "sys_data_object")) %>%
      dplyr::filter(is.na(exist_s3))

    df_missing$name

  #create a temp folder for storing downloads
    tmp <- fs::dir_create(fs::file_temp())
    cat("downloaded files saved to", tmp)

  #download all files missing from s3 locally
    purrr::walk2(.x = df_missing$id,
                 .y = df_missing$name,
                 .f = ~googledrive::drive_download(googledrive::as_id(.x),
                                                   file.path(tmp, .y),
                                                   overwrite = TRUE))

# UPLOAD SUBMISSION FILES to S3

    #identify file paths downloaded from Gdrive to push to s3 bucket
      upload_files <- list.files(tmp, full.names = TRUE)

    #upload one file to kick off
      glamr::s3_upload(
        file = tail(upload_files, n=1),
        bucket = hfr_bucket,
        prefix = "ddc/uat/raw/hfr/incoming")

    #push local files to s3
      purrr::walk(head(upload_files, -1),
                  ~ glamr::s3_upload(
                    file = .,
                    bucket = hfr_bucket,
                    prefix = "ddc/uat/raw/hfr/incoming"))


# TODO - SCHEDULE CRON JOBS TO PROCESSIONS DATA

    #identify how many trifacta runs are needed (30 tabs per run)
      tabs <- purrr::map_dfr(upload_files,
                             ~ readxl::excel_sheets(.) %>%
                               tibble::as_tibble() %>%
                               dplyr::filter(str_detect(value, "HFR")))

      cat("Trifacta runs needed:", ceiling(nrow(tabs)/30))


# DOWNLOAD TABLEAU OUTPUTS ------------------------------------------------

  # HFR Data Process dates
    # proc_dates <- "2021-02-01" %>% as.Date()

  #FY21 HFR Outputs
    (files_twbx <- s3_objects(
        bucket = hfr_bucket,
        prefix = "ddc/uat/processed/hfr/outgoing/HFR_Tableau"
      ) %>%
      s3_unpack_keys() %>%
      # filter(
      #   str_detect(
      #     sys_data_object,
      #     pattern = "^hfr_2021_\\d{2}_Tableau_\\d{4}-\\d{2}-\\d{2}.csv$"),
      #   last_modified %in% proc_dates
      # ) %>%
      filter(last_modified == max(last_modified)) %>%
      pull(key))

    files_twbx %>%
      map(.x, .f = ~ s3_download(
        bucket = hfr_bucket,
        object = .x,
        filepath = file.path("./out/joint", "HFR_Tableau_SQLview.csv")
      ))


# DOWNLOAD ERROR REPORTS

  # HFR Reporting Period
    proc_dates <- "2021-02-18" %>% as.Date()
    from_pd <- "202101"
    curr_pd <- "202104"

    #pds <- glue::glue("{from_pd}_{curr_pd}")
    pds <- glue::glue("{curr_pd}_{curr_pd}")

  #HFR Submissions status
    s3_objects(
        bucket = hfr_bucket,
        prefix = "ddc/uat/processed/hfr/outgoing/HFR_Submission"
      ) %>%
      s3_unpack_keys() %>%
      filter(
        str_detect(
          str_to_lower(sys_data_object),
          # pattern = paste0("^hfr_submission_status_", pds, "_\\d{4}-\\d{2}-\\d{2}.csv$"))
          pattern = "^hfr_submission_status_2021-")
      ) %>%
      pull(key) %>%
      sort() %>%
      last() %>%
      map(.x, .f = ~ s3_download(
        bucket = hfr_bucket,
        object = .x,
        filepath = file.path("./out/DDC", basename(.x))
      ))

  #HFR Mechanisms Status:
    s3_objects(
        bucket = hfr_bucket,
        prefix = "ddc/uat/processed/hfr/outgoing/Mechanism", # This path has changed
        n = 10000
      ) %>%
      s3_unpack_keys() %>%
      # filter(
      #   str_detect(
      #     str_to_lower(sys_data_object),
      #     pattern = "^mechanism_detail_output_\\d{4}-\\d{2}-\\d{2}.csv$")
      # ) %>%
      pull(key) %>%
      sort() %>%
      last() %>%
      map(.x, .f = ~ s3_download(
        bucket = hfr_bucket,
        object = .x,
        filepath = file.path("./out/DDC", basename(.x))
      ))

  #HFR Detailed Errros
    s3_objects(
      bucket = hfr_bucket,
      prefix = "ddc/uat/processed/hfr/outgoing/Detailed"
    ) %>%
      s3_unpack_keys() %>%
      # filter(
      #   str_detect(
      #     str_to_lower(sys_data_object),
      #     pattern = "^detailed_error_output_\\d{4}-\\d{2}-\\d{2}.csv$"),
      #   last_modified %in% proc_dates
      # ) %>%
      pull(key) %>%
      sort() %>%
      last() %>%
      map(.x, .f = ~ s3_download(
        bucket = hfr_bucket,
        object = .x,
        filepath = file.path("./out/DDC", basename(.x))
      ))



# RETIRED - PROCESS HFR SUBMISSIONS (WAVELENGTH) --------------------------

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

