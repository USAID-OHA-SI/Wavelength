## PROJECT:  HFR
## AUTHOR:   A.CHAFETZ | USAID
## PURPOSE:  combine submitted FY21 reporting sites
## LICENSE:  MIT
## DATE:     2020-11-03
## UPDATED:  2021-03-05



# IMPORT ------------------------------------------------------------------

  #local folder containing submission from HFR Google Drive
    # googledrive::drive_browse(googledrive::as_id("1taML0yih_ZcDL4Jr8Bus3mCFD2S92BK7"))
    fldr_submissions <- "../../../Downloads/3.4 FY21 Site Lists"

  #file names of submission to read in
    files_submissions <- list.files(fldr_submissions, full.names = TRUE)

  #import files
    df_submissions <- purrr::map_dfr(files_submissions, tidy_sitelist)

  #local folder with the site list created from FY20
    fldr_orig <- "out/siteval"

  #list files in original folder
    files_orig <- list.files(fldr_orig, full.names = TRUE)

  #import original files
    df_orig <- purrr::map_dfr(files_orig, tidy_sitelist)

# MUNGE -------------------------------------------------------------------

  #create a flag, is_orginal (FY20) is FALSE since these are submissions
    df_submissions <- df_submissions %>%
      dplyr::select(-Column1) %>%
      dplyr::mutate(is_original = FALSE)

  #list of submitted ous (ou/countryname) to filter out of the originals
    submission_ous <- unique(df_submissions$operatingunit)

  #list of original ous
    orig_ous <- unique(df_orig$operatingunit)

  #filter out submitted ous and flag as original
    df_orig <- df_orig %>%
      dplyr::filter(!(operatingunit %in% submission_ous)) %>%
      dplyr::mutate(is_original = TRUE)

  #bind two together
    df_sitelist <- df_orig %>%
      dplyr::bind_rows(df_submissions) %>%
      dplyr::arrange(operatingunit, orgunit)

  #parse operatingunit for ou and country
    df_sitelist <- df_sitelist %>%
      tidyr::separate(operatingunit, c("operatingunit", "countryname"), sep = "/", fill = "right") %>%
      dplyr::mutate(countryname = ifelse(is.na(countryname), operatingunit, countryname))


# EXPORT ------------------------------------------------------------------

  date <- format(Sys.Date(), "%Y%m%d")

  file <- paste0("HFR_FY21_GLOBAL_sitelist_", date, ".csv")

  readr::write_csv(df_sitelist,
                   file.path("out", file), na = "")



# UPLOAD REFERENCE TABLES TO S3 AND GDRIVE --------------------------------

  #load creds for google and s3
  glamr::load_secrets()

  #upload to google drive
  googledrive::drive_upload(file.path("out", file),
                            googledrive::as_id("12bah06bx71-EPa0mdPiwOGCROOliJX85"),
                            name = file)

  # upload to s3 bucket
  list.files(
    path = "out",
    pattern = file,
    full.names = TRUE
  ) %>%
    sort() %>%
    dplyr::last() %>%
    glamr::s3_upload(
      bucket = "gov-usaid",
      prefix = "ddc/uat/raw/hfr/receiving"
    )
