#' #' Append HFR and DATIM Data
#'
#' @param folderpath_hfr folder path to HFR processed data
#' @param folderpath_datim folder path to DATIM extracts, see `extract_datim()`
#' @param start_date start date of HFR period, YYYY-MM-DD format
#' @param weeks number of weeks to create, default = 4
#' @param max_date cut off data at max date? default = NULL
#' @param folderpath_output folder path for saving the output
#'
#' @export
#'

hfr_append_sources <- function(folderpath_hfr,
                           folderpath_datim,
                           start_date,
                           weeks = 4,
                           max_date = TRUE,
                           folderpath_output){

  # IMPORT ------------------------------------------------------------------

  #ensure pulling the lastest HFR files
  hfr_files_newest <- list.files(folderpath_hfr, full.names = TRUE) %>%
    tibble::enframe(name = NULL, value = "full_path") %>%
    dplyr::mutate(name = basename(full_path) %>% stringr::str_remove_all("HF(R|D)_|\\.csv")) %>%
    tidyr::separate(name, c(NA, "ou", "date"), sep = "_", extra = "drop") %>%
    dplyr::mutate(date = lubridate::as_date(date)) %>%
    dplyr::arrange(ou) %>%
    dplyr::group_by(ou) %>%
    dplyr::mutate(newest = date == max(date)) %>%
    dplyr::filter(newest) %>%
    dplyr::pull(full_path)

  #pull in HFR data
  df_hfr <- purrr::map_dfr(.x = hfr_files_newest,
                           .f = ~ readr::read_csv(.x, col_types = c(.default = "c")))

  #pull in DATIM target files
  df_datim <- purrr::map_dfr(.x = list.files(folderpath_datim, full.names = TRUE),
                             .f = ~ readr::read_tsv(.x, col_types = c(.default = "c")))
  df_datim <- df_datim %>%
    dplyr::select(-dplyr::matches("Type of organisational unit"))


# EXTRACT MISSING PARTNER NAME --------------------------------------------

  if(!"primepartner" %in% names(df_datim)){
    #access current mechanism list posted publically to DATIM
    sql_view_url <- "https://www.datim.org/api/sqlViews/fgUtV6e9YIX/data.csv"
    mech_official <- readr::read_csv(sql_view_url,
                                     col_types = readr::cols(.default = "c"))

    mech_info <- mech_official %>%
      dplyr::filter(agency == "USAID") %>%
      dplyr::select(mech_code = code, primepartner_d = partner)

    df_datim <- df_datim %>%
      tibble::add_column(primepartner = as.character(NA), .before = "mech_code") %>%
      dplyr::left_join(mech_info, by = "mech_code") %>%
      dplyr::mutate(primepartner = primepartner_d) %>%
      dplyr::select(-primepartner_d) %>%
      dplyr::glimpse()

    rm(sql_view_url, mech_official, mech_info)
  }


# MAP MECHANISM INFORMATION -----------------------------------------------

  #pull list of mechanism from DATIM targets
  df_mech_map <- df_datim %>%
    dplyr::distinct(mech_code, primepartner, mech_name) %>%
    dplyr::rename_at(dplyr::vars(primepartner, mech_name), ~ paste0(., "_d"))

  #merge on mech_code, replacing HFR with DATIM names
  df_hfr <- df_hfr %>%
    dplyr::left_join(df_mech_map, by = "mech_code") %>%
    dplyr::mutate(primepartner = primepartner_d,
                  mech_name = mech_name_d) %>%
    dplyr::select(-dplyr::ends_with("_d"))

  rm(df_mech_map)

  # MAP ORG HIERARCHY -------------------------------------------------------

  #pull list of org hierarchy from DATIM targets
  df_org_map <- df_datim %>%
    dplyr::distinct(orgunit, orgunituid, snu1, psnu) %>%
    dplyr::rename_all(~ paste0(., "_d"))

  #merge for those with facility uids
  df_hfr_orguids <- df_hfr %>%
    dplyr::filter(!is.na(orgunituid)) %>%
    dplyr::left_join(df_org_map, by = c("orgunituid" = "orgunituid_d")) %>%
    dplyr::mutate(#snu1 = ifelse(!is.na(snu1_d), snu1_d, snu1),
                  psnu = ifelse(!is.na(psnu_d), psnu_d, psnu),
                  orgunit = ifelse(!is.na(orgunit_d), orgunit_d, orgunit)) %>%
    dplyr::select(-dplyr::ends_with("_d"))

  #merge for those without facility uids
  df_org_map_missing <- dplyr::distinct(df_org_map, orgunit_d, .keep_all= TRUE)
  df_hfr_orguids_missing <- df_hfr %>%
    dplyr::filter(is.na(orgunituid)) %>%
    dplyr::left_join(df_org_map_missing, by = c("orgunit" = "orgunit_d")) %>%
    dplyr::mutate(#snu1 = ifelse(!is.na(snu1_d), snu1_d, snu1),
                  psnu = ifelse(!is.na(psnu_d), psnu_d, psnu),
                  orgunituid = orgunituid_d) %>%
    dplyr::select(-dplyr::ends_with("_d"))

  #append df with org hierarchy together
  df_hfr <- dplyr::bind_rows(df_hfr_orguids, df_hfr_orguids_missing)

  rm(df_hfr_orguids, df_hfr_orguids_missing, df_org_map, df_org_map_missing)

  # REMOVE ENDING MECHANISMS ------------------------------------------------

  # #access current mechanism list posted publically to DATIM
  # sql_view_url <- "https://www.datim.org/api/sqlViews/fgUtV6e9YIX/data.csv"
  # mech_official <- readr::read_csv(sql_view_url,
  #                                  col_types = readr::cols(.default = "c"))
  #
  # #rename variables to match MSD and remove mechid from mech name
  # ending_mechs <- mech_official %>%
  #   dplyr::filter(agency == "USAID") %>%
  #   dplyr::mutate(enddate = lubridate::ymd(enddate)) %>%
  #   dplyr::select(mech_code = code, enddate) %>%
  #   dplyr::filter(lubridate::year(enddate) < 2020) %>%
  #   dplyr::pull(mech_code)
  #
  # df_datim <- dplyr::filter(df_datim, !mech_code %in% ending_mechs)
  #
  # rm(sql_view_url, mech_official, ending_mechs)

  # DUPICATE TARGETS --------------------------------------------------------

  #clean up age and sex
  df_datim <- df_datim %>%
    dplyr::mutate(agecoarse = stringr::str_remove(agecoarse, " Age"),
                  sex = stringr::str_remove(sex, " sex"))

  #duplicate targets for each week (DATIM)
  dates <- lubridate::as_date(start_date) %>% seq(by = 7, length.out = weeks)
  df_datim_rpt <- purrr::map_dfr(.x = dates,
                                 .f = ~dplyr::mutate(df_datim, date = .x)) %>%
    hfr_assign_pds()

  rm(df_datim, dates)

  # APPEND HFR AND TARGETS --------------------------------------------------

  df_hfr <- df_hfr %>%
    dplyr::mutate(#date = lubridate::mdy(date),
                  date = as.Date(date),
                  fy = as.integer(fy),
                  hfr_pd = as.integer(hfr_pd),
                  sex = ifelse(sex == "Unknown", "Unspecified", sex),
                  operatingunit = ifelse(operatingunit == "DRC","Democratic Republic of the Congo", operatingunit),
                  operatingunit = ifelse(operatingunit == "Viet Nam","Vietnam", operatingunit)) %>%
    #assign_pds() %>%
    dplyr::bind_rows(df_datim_rpt)

  #aggregate to reduce # of lines
  sum_vars <- c("mer_results", "mer_targets", "targets_gap", "weekly_targets", "weekly_targets_gap", "val")

  df_hfr <- df_hfr %>%
    dplyr::mutate_at(dplyr::vars(sum_vars), as.numeric) %>%
    dplyr::group_by_at(setdiff(names(df_hfr), c("partner", "disaggregate", sum_vars))) %>%
    dplyr::summarise_at(dplyr::vars(sum_vars), sum, na.rm = TRUE) %>%
    dplyr::ungroup()

  rm(df_datim_rpt, sum_vars)

  #cap date at last date of reporting of collection period
  if(max_date == TRUE){
    max <- as.Date(start_date)+(7*(weeks-1))
    df_hfr <- dplyr::filter(df_hfr, date <= as.Date(max))
  }


# ARRANGE COLUMNS ---------------------------------------------------------

  df_hfr <- dplyr::select(df_hfr,
                          operatingunit, orgunit, orgunituid, date, fy, hfr_pd,
                          mech_code, mech_name, primepartner, fundingagency, snu1,
                          psnu, community, indicator, agecoarse, sex, otherdisaggregate,
                          val, mer_results, mer_targets, targets_gap, weekly_targets,
                          weekly_targets_gap, dplyr::everything())

# EXPORT ------------------------------------------------------------------


  if(!is.null(folderpath_output)){

    pd <- df_hfr %>%
      dplyr::distinct(fy, hfr_pd) %>%
      dplyr::mutate(pd = fy + (hfr_pd/100)) %>%
      dplyr::pull(pd) %>%
      max()

    readr::write_csv(df_hfr,
                     file.path(folderpath_output,
                               paste0("HFR_",
                                      pd, "_Global_output_",
                                      format(Sys.time(),"%Y%m%d.%H%M"),
                                      ".csv")),
                     na = "")
  }



  invisible(df_hfr)

  }
