# Pull from DATIM ---------------------------------------------------------

  ## NOTE: Needs to be updated each quarter for updated Results/Targets/Gap Target

  #DATIM username
    myuser <- ""

  #quarters with results in DATIM
    qtr <- 2

  #folderpath output
    savefolder <- "out/DATIM"

  #pull MER data
    ous <- identify_levels(username = myuser, password = mypwd(myuser)) %>% dplyr::pull(country_name)
    purrr::walk(ous, ~pull_mer(ou_name = .x,
                                    username = myuser,
                                    password = mypwd(myuser),
                                    quarters_complete = qtr,
                                    folderpath_output = savefolder))
  #pull hierarchy
    ouuids <- identify_ouuids(myuser, mypwd(myuser)) %>% dplyr::pull(id)
    df_orgs <- purrr::map_dfr(.x = ouuids,
                              .f = ~ pull_hierarchy(.x, myuser, mypwd(myuser)))
    hfr_export(df_orgs, savefolder, type = "orghierarchy")

  #pull mechanism info
    pull_mech(folderpath_output = "out/DATIM")

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

