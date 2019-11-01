# Pull from DATIM ---------------------------------------------------------

  ## NOTE: Needs to be updated each quarter for updated Results/Targets/Gap Target

  #DATIM username
    myuser <- "achafetz_global"

  #identify full set of OU/country list for all USAID targets
    ous <- identify_levels(username = myuser, password = mypwd(myuser)) %>%
      dplyr::filter(is.na(name4)) %>%
      dplyr::pull(country_name)

    ous <- c(ous, "Laos","Thailand", 'Guatemala', "Hondura", "Nicaragua", "Panama")

  #extract and save targets for all OUs
    purrr::walk(ous, ~extract_datim(ou_name = .x,
                                    username = myuser,
                                    password = mypwd(myuser),
                                    quarters_complete = 3,
                                    folderpath_output = "out/DATIM"))


# Combine HFR & DATIM -----------------------------------------------------

  ## NOTE: this creates a global file,

  #folder where all HFR processed files are stored (will determine latest verion by OU)
    path <- "../Downloads/HFR_FILES"

  #append DATIM onto HFR (map so completes for all OUs in folderpath_hfr)
    df <- append_sources(folderpath_hfr = path,
                         folderpath_datim = "out/DATIM",
                         start_date = "2019-06-03",
                         weeks = 13,
                         folderpath_output = "out/joint")

# Integrity Checks --------------------------------------------------------

    path <- "../Downloads/PD 12"
    files <- list.files(path, full.names = TRUE)

    #import
    df_check <- purrr::map_dfr(files,
                               ~ readr::read_csv(.x, col_types = c(.default = "c"))) %>%
      dplyr::mutate(val = as.double(val),
                    date = lubridate::mdy(date))


    #check headers
    check_headers <- function(filepath){

       okay_names <- c("otherdisaggregate", "mechanismid",
                       "operatingunit", "orgunituid", "snu1",
                       "psnu", "orgunit", "val", "fundingagency",
                       "partner", "date", "indicator", "agecoarse",
                       "sex")
        df <- readr::read_csv(filepath, col_types = c(.default = "c"))

        ou <- unique(df$operatingunit)

        bad_names <- setdiff(names(df), okay_names)

        if(length(bad_names) == 0) {
          bad_names <- "all headers okay"
        } else if (length(bad_names > 50)) {
          bad_names <- paste(bad_names, collapse = ", ") %>% stringr::str_sub(end = 50)
        } else {
          bad_names <- paste(bad_names, collapse = ", ")
        }

        print(paste0(ou, ": ", bad_names))

      }

    #included mecahnism ids?
    check_mechid <- function(df){

      total <- df %>%
        dplyr::distinct(mechanismid, partner) %>%
        nrow()

      missing <- df %>%
        dplyr::distinct(mechanismid, partner) %>%
        dplyr::filter(is.na(mechanismid)) %>%
        nrow()

      if(missing == 0){
        print("All partners have mechanism IDs")
      } else {
        print(paste0(missing, " of the ", total, " mechanisms are missing DATIM mechanism IDs." ))
      }
    }

    #included facility uids
    check_orgunituid <- function(df){

      total <- df %>%
        dplyr::distinct(facility, orgunituid) %>%
        nrow()

      missing <- df %>%
        dplyr::distinct(facility, orgunituid) %>%
        dplyr::filter(is.na(orgunituid)) %>%
        nrow()

      if(missing == 0){
        print("All partners have sites/communities UIDs")
      } else {
        print(paste0(missing, " of the ", total, " sites/communities are missing DATIM UIDs." ))
      }
    }

    #indicators
    check_ind <- function(df){

      df_fullset <- tibble::tribble(
        ~indicator, ~agecoarse,     ~sex, ~otherdisaggregate,
        "HTS_TST",      "<15", "Female",                 NA,
        "HTS_TST",      "15+", "Female",                 NA,
        "HTS_TST",      "<15",   "Male",                 NA,
        "HTS_TST",      "15+",   "Male",                 NA,
        "HTS_TST_POS",      "<15", "Female",                 NA,
        "HTS_TST_POS",      "15+", "Female",                 NA,
        "HTS_TST_POS",      "<15",   "Male",                 NA,
        "HTS_TST_POS",      "15+",   "Male",                 NA,
        "TX_NEW",      "<15", "Female",                 NA,
        "TX_NEW",      "15+", "Female",                 NA,
        "TX_NEW",      "<15",   "Male",                 NA,
        "TX_NEW",      "15+",   "Male",                 NA,
        "TX_CURR",      "<15", "Female",                 NA,
        "TX_CURR",      "15+", "Female",                 NA,
        "TX_CURR",      "<15",   "Male",                 NA,
        "TX_CURR",      "15+",   "Male",                 NA,
        "VMMC_CIRC",      "<15",   "Male",                 NA,
        "VMMC_CIRC",      "15+",   "Male",                 NA,
        "PrEP_NEW",      "15+", "Female",                 NA,
        "PrEP_NEW",      "15+",   "Male",                 NA,
        "TX_MMD",         NA,       NA,          "1 month",
        "TX_MMD",         NA,       NA,         "2 months",
        "TX_MMD",         NA,       NA,         "3 months",
        "TX_MMD",         NA,       NA,       "4-5 months",
        "TX_MMD",         NA,       NA, "6 or more months"
      )

      missing <- df_fullset %>%
        dplyr::distinct(indicator) %>%
        dplyr::pull() %>%
        setdiff(unique(df$indicator)) %>%
        paste(collapse = ", ")

      if(missing == 0){
        print("No indicators are missing.")
      } else {
        print(paste0("Missing indicators: ", missing))
      }
    }

    #check # of sites reporting each indicators and disagg
    check_inddisagg <- function(df, pd_start_date = NULL){

      # df <- readr::read_csv(filepath, col_types = c(.default = "c")) %>%
      #   dplyr::mutate(val = as.double(val))

      df_fullset <- tibble::tribble(
            ~indicator, ~agecoarse,     ~sex, ~otherdisaggregate, ~check,
             "HTS_TST",      "<15", "Female",                 NA, "okay",
             "HTS_TST",      "15+", "Female",                 NA, "okay",
             "HTS_TST",      "<15",   "Male",                 NA, "okay",
             "HTS_TST",      "15+",   "Male",                 NA, "okay",
         "HTS_TST_POS",      "<15", "Female",                 NA, "okay",
         "HTS_TST_POS",      "15+", "Female",                 NA, "okay",
         "HTS_TST_POS",      "<15",   "Male",                 NA, "okay",
         "HTS_TST_POS",      "15+",   "Male",                 NA, "okay",
              "TX_NEW",      "<15", "Female",                 NA, "okay",
              "TX_NEW",      "15+", "Female",                 NA, "okay",
              "TX_NEW",      "<15",   "Male",                 NA, "okay",
              "TX_NEW",      "15+",   "Male",                 NA, "okay",
             "TX_CURR",      "<15", "Female",                 NA, "okay",
             "TX_CURR",      "15+", "Female",                 NA, "okay",
             "TX_CURR",      "<15",   "Male",                 NA, "okay",
             "TX_CURR",      "15+",   "Male",                 NA, "okay",
           "VMMC_CIRC",      "<15",   "Male",                 NA, "okay",
           "VMMC_CIRC",      "15+",   "Male",                 NA, "okay",
            "PrEP_NEW",      "15+", "Female",                 NA, "okay",
            "PrEP_NEW",      "15+",   "Male",                 NA, "okay",
              "TX_MMD",         NA,       NA,          "1 month", "okay",
              "TX_MMD",         NA,       NA,         "2 months", "okay",
              "TX_MMD",         NA,       NA,         "3 months", "okay",
              "TX_MMD",         NA,       NA,       "4-5 months", "okay",
              "TX_MMD",         NA,       NA, "6 months or more", "okay"
         )


      pd_start_date <- as.Date(pd_start_date)
      pd_end_date <- as.Date(pd_start_date) + 21

      df %>%
        dplyr::mutate(date = lubridate::mdy(date)) %>%
        dplyr::group_by(operatingunit, orgunit, orgunituid, indicator, agecoarse, sex, otherdisaggregate, date) %>%
        dplyr::summarise_at(dplyr::vars(val), sum, na.rm = TRUE) %>%
        dplyr::ungroup() %>%
        dplyr::filter(date >= pd_start_date,
                      date <= pd_end_date) %>%
        dplyr::count(operatingunit, indicator, agecoarse, sex, otherdisaggregate, date) %>%
        tidyr::spread(date, n) %>%
        dplyr::full_join(df_fullset, ., by = c("indicator", "agecoarse", "sex", "otherdisaggregate")) %>% #"otherdisaggregate"
        dplyr::mutate(disagg = dplyr::case_when(indicator == "TX_MMD" ~ otherdisaggregate,
                                                TRUE                  ~ paste(sex, agecoarse, sep = "/"))) %>%
        dplyr::select(-agecoarse, -sex, -otherdisaggregate) %>%
        dplyr::select(operatingunit, indicator, disagg, dplyr::everything()) %>%
        dplyr::arrange(operatingunit, indicator, disagg) %>%
        print(n = Inf)
    }


    purrr::walk(files, check_headers)
    #purrr::walk(files[3], check_inddisagg, "2019-08-05")
    check_inddisagg(df_check, "2019-08-05")

    check_mechid(df)
    check_orgunituid(df)
    check_ind(df)
    check_inddisagg(df, "2019-08-05") %>% dplyr::filter(is.na(check)) %>% print(n = Inf)


    #check indicators

    ind_okay <- c("HTS_TST", "HTS_TST_POS", "TX_NEW", "TX_CURR", "TX_MMD", "PrEP_NEW", "VMMC_CIRC")
    sex_okay <- c("Male", "Female", "Unknown", NA)
    age_okay <- c("<15", "15+", NA)
    otherdisagg_okay <- c("1 month", "2 months", "3 months", "4-5 months", "6 months or more", NA)

    df_check %>%
      dplyr::filter(!indicator %in% ind_okay) %>%
      dplyr::count(operatingunit, indicator)

    #check sex
    df_check %>%
      dplyr::filter(!sex %in% sex_okay) %>%
      dplyr::count(operatingunit, sex)

    df_check %>%
      dplyr::count(sex, agecoarse)

    #age
    df_check %>%
      dplyr::filter(!agecoarse %in% age_okay) %>%
      dplyr::count(operatingunit, agecoarse)

    #check_dates
      df_check %>%
        dplyr::mutate(operatingunit = ifelse(stringr::str_detect(operatingunit, "Region"),
                                             paste(operatingunit, snu1, sep = "/"), operatingunit)) %>%
        dplyr::filter(date < "2019-09-02") %>%
        dplyr::count(operatingunit, date) %>%
        assign_pds() %>%
        dplyr::filter(is.na(hfr_pd)) %>%
        dplyr::select(-c(fy, hfr_pd)) %>%
        print(n = Inf)

    #check other disaggregate
      df_check %>%
        dplyr::filter(!otherdisaggregate %in% otherdisagg_okay) %>%
        assign_pds() %>%
        dplyr::count(operatingunit, indicator, hfr_pd, otherdisaggregate) %>%
        print(n = Inf)


      df_check %>%
        assign_pds() %>%
        dplyr::count(operatingunit, date, hfr_pd) %>%
        dplyr::arrange(operatingunit, date) %>%
        print(n = Inf)

      df_check %>%
        dplyr::count(operatingunit, date) %>%
        dplyr::arrange(operatingunit, date) %>%
        print(n = Inf)
