# Pull from DATIM ---------------------------------------------------------

  ## NOTE: Needs to be updated each quarter for updated Results/Targets/Gap Target

  #DATIM username
    myuser <- ""

  #identify full set of OU/country list for all USAID targets
    ous <- identify_levels(username = myuser, password = mypwd(myuser)) %>%
      dplyr::filter(is.na(name4)) %>%
      dplyr::pull(country_name)


    ous <- identify_levels(username = myuser, password = mypwd(myuser)) %>%
      dplyr::filter(!is.na(name4)) %>%
      dplyr::pull(country_name) %>%
      sort()

    ous <- c('Guatemala', "Hondura",
             "Nicaragua", "Panama")

  #extract and save targets for all OUs
    purrr::walk(ous, ~extract_datim(ou_name = .x,
                                    username = myuser,
                                    password = mypwd(myuser),
                                    quarters_complete = 3,
                                    folderpath_output = "out/DATIM"))


# Combine HFR & DATIM -----------------------------------------------------

  ## NOTE: this creates a global file,

  #folder where all HFR processed files are stored
    path <- "../Downloads/HFR_PD11n"

  #append DATIM onto HFR (map so completes for all OUs in folderpath_hfr)
    df <- append_sources(folderpath_hfr = path,
                         folderpath_datim = "out/DATIM",
                         start_date = "2019-06-03",
                         weeks = 9,
                         max_date = "2019-07-29",
                         folderpath_output = "out/joint")


# Integrity Checks --------------------------------------------------------

  #check headers
    names(df)

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
    check_inddisagg <- function(df, pd_start_date){

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

      pd_start_date <- as.Date(pd_start_date)
      pd_end_date <- as.Date(pd_start_date) + 28

      df %>%
        dplyr::group_by(facility, orgunituid, indicator, agecoarse, sex, otherdisaggregate, date) %>%
        dplyr::summarise_at(dplyr::vars(val), sum, na.rm = TRUE) %>%
        dplyr::ungroup() %>%
        dplyr::filter(date >= pd_start_date,
                      date <=  pd_end_date) %>%
        dplyr::count(indicator, agecoarse, sex, otherdisaggregate, date) %>%
        tidyr::spread(date, n) %>%
        dplyr::full_join(df_fullset, ., by = c("indicator", "agecoarse", "sex", "otherdisaggregate")) %>% #"otherdisaggregate"
        dplyr::mutate(disagg = dplyr::case_when(indicator == "TX_MMD" ~ otherdisaggregate,
                                                TRUE                  ~ paste(sex, agecoarse, sep = "/"))) %>%
        dplyr::select(-agecoarse, -sex, -otherdisaggregate) %>%
        dplyr::select(indicator, disagg, dplyr::everything()) %>%
        print(n = Inf)
    }


    check_mechid(df)
    check_orgunituid(df)
    check_ind(df)
    check_inddisagg(df, "2019-05-13")




