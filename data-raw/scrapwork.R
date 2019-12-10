# Pull from DATIM ---------------------------------------------------------

  ## NOTE: Needs to be updated each quarter for updated Results/Targets/Gap Target

  #DATIM username
    myuser <- ""

  #pull MER data
    ous <- identify_levels(username = myuser, password = mypwd(myuser)) %>% dplyr::pull(country_name)
    purrr::walk(ous, ~pull_mer(ou_name = .x,
                                    username = myuser,
                                    password = mypwd(myuser),
                                    quarters_complete = 0,
                                    folderpath_output = "out/DATIM"))
  #pull hierarchy
    ouuids <- identify_ouuids(myuser, mypwd(myuser)) %>% dplyr::pull(id)
    df_orgs <- purrr::map_dfr(.x = ouuids,
                              .f = ~ pull_hierarchy(.x, myuser, mypwd(myuser)))
    hfr_export(df_orgs, "out/DATIM", type = "orghierarchy")

  #pull mechanism info
    pull_mech(folderpath_output = "out/DATIM")

# Process HFR submissions -------------------------------------------------

    (files <- list.files("ou_submissions", full.names = TRUE))

    df_hfr <- purrr::map_dfr(files, ~ hfr_process_template(.x, round_hfrdate = TRUE))

    df_hfr %>%
      readr::write_csv(paste0("out/processed/HFR_2020.01_Global_",format(Sys.Date(), "%Y%m%d"), ".csv"), na = "")

# Combine HFR & DATIM -----------------------------------------------------

  ## NOTE: this creates a global file,

  #folder where all HFR processed files are stored (will determine latest verion by OU)
    path <- "../Downloads/HFR_FILES"

  #append DATIM onto HFR (map so completes for all OUs in folderpath_hfr)
    df <- append_sources(folderpath_hfr = "out/processed",
                         folderpath_datim = "out/DATIM",
                         start_date = "2019-09-30",
                         weeks = 4,
                         max_date = TRUE,
                         folderpath_output = "out/joint")

