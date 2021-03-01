# OU UIDs -----------------------------------------------------------------

#' Pull OU UIDS
#'
#' @param baseurl base url for the API, default = https://final.datim.org/
#' @param username DATIM Username
#' @param password DATIM password, recommend using `mypwd()`
#'
#' @export
#'
#' @examples
#' \dontrun{
#'  ous <- identify_ouuids("userx", mypwd("userx")) }

identify_ouuids <- function(username, password, baseurl = "https://final.datim.org/"){

  package_check("httr")
  package_check("jsonlite")

  ous <- baseurl %>%
    paste0("api/organisationUnits?filter=level:eq:3") %>%
    httr::GET(httr::authenticate(username,password)) %>%
    httr::content("text") %>%
    jsonlite::fromJSON(flatten=TRUE) %>%
    purrr::pluck("organisationUnits")

  region_uids <- ous %>%
    dplyr::filter(stringr::str_detect(displayName, "Region")) %>%
    dplyr::pull(id)

  ctrys <- purrr::map_dfr(.x = region_uids,
                          .f = ~ baseurl %>%
                            paste0("api/organisationUnits?filter=level:eq:4&filter=path:like:", .x) %>%
                            httr::GET(httr::authenticate(username,password)) %>%
                            httr::content("text") %>%
                            jsonlite::fromJSON(flatten=TRUE) %>%
                            purrr::pluck("organisationUnits") %>%
                            dplyr::filter(stringr::str_detect(displayName, "Region", negate = TRUE)) %>%
                            dplyr::mutate(regional = TRUE))


  uids <- ous %>%
    dplyr::bind_rows(ctrys) %>%
    dplyr::arrange(displayName)

  return(uids)
}


# Identify Levels ---------------------------------------------------------

#' Identify Facility/Community levels in org hierarchy
#'
#' @param ou operating unit name
#' @param username DATIM username
#' @param password DATIM password, recommend using `mypwd()`
#' @param baseurl base API url, default = https://final.datim.org/
#'
#' @export
#'
#' @examples
#' \dontrun{
#'  #table for all OUs
#'   myuser <- "UserX"
#'   identify_levels(username = myuser, password = mypwd())
#'  #table for just Kenya
#'    identify_levels("Kenya", username = myuser, password = mypwd()) }

identify_levels <- function(ou = NULL, username, password, baseurl = "https://final.datim.org/"){

  package_check("httr")
  package_check("jsonlite")

  df_levels <- baseurl %>%
    paste0(.,"api/dataStore/dataSetAssignments/orgUnitLevels") %>%
    httr::GET(httr::authenticate(username,password)) %>%
    httr::content("text") %>%
    jsonlite::fromJSON(flatten=TRUE) %>%
    purrr::map_dfr(dplyr::bind_rows) %>%
    dplyr::mutate_if(is.character, ~ dplyr::na_if(., ""))

  #adjust for regional missions
  df_levels <- df_levels %>%
    dplyr::mutate(country_name = ifelse(is.na(name4), name3, name4))

  if(!is.null(ou))
    df_levels <- dplyr::filter(df_levels, country_name == ou)

  return(df_levels)
}


# Pull Target Data from DATIM ---------------------------------------------

#' DATIM API Call for Targets
#'
#' @param url supply url forAPI call, recommend using`gen_url()`
#' @param username DATIM username
#' @param password DATIM password, recommend using `mypwd()`
#'
#' @export
#'
#' @examples
#' \dontrun{
#'  myurl <- paste0(baseurl, "api/29/analytics.json?
#'                  dimension=LxhLO68FcXm:udCop657yzi&
#'                  dimension=ou:LEVEL-4;HfVjCurKxh2&
#'                  filter=pe:2018Oct&
#'                  displayProperty=SHORTNAME&outputIdScheme=CODE")
#'  myuser <- "UserX"
#'  df_targets <- get_datim_targets(myurl, myuser, mypwd(myuser)) }

get_datim_targets <- function(url,username,password) {

  package_check("httr")
  package_check("jsonlite")

  json <- url %>%
    httr::GET(httr::authenticate(username,password)) %>%
    httr::content("text") %>%
    jsonlite::fromJSON()

  if ( NROW(json$rows) > 0 ) {
    metadata <- purrr::map_dfr(json$metaData$items, dplyr::bind_rows, .id = "from")

    df <- tibble::as_tibble(json$rows, .name_repair = ~ json$headers$column)

    orguids <- df$`Organisation unit`

    if(stringr::str_detect(url, "hierarchyMeta=true")){

      orgpath <- dplyr::bind_rows(json$metaData$ouHierarchy) %>%
        tidyr::gather()

      levels <- orgpath$value %>%
        stringr::str_count("/") %>%
        max() + 1

      headers <- paste0("orglvl_", seq(1:levels))

      df <- dplyr::left_join(df, orgpath, by = c("Organisation unit" = "key")) %>%
        tidyr::separate(value, headers, sep = "/")
    }


    df <- df %>%
      dplyr::mutate_all(~plyr::mapvalues(., metadata$from, metadata$name, warn_missing = FALSE)) %>%
      dplyr::mutate(Value = as.numeric(Value)) %>%
      dplyr::bind_cols(orgunituid = orguids)

    return(df)

  } else {

    return(NULL)

  }
}

# Compile URL -------------------------------------------------------------

#' Generate a API URL
#'
#' @param ou_uid UID for the country, recommend using `identify_ouuids()`
#' @param org_lvl org hierarchy level, eg facility is level 7 in country X, recommend using `identify_levels()`
#' @param org_type organization type, either facility (default) or community
#' @param value_type results (default) or targets
#' @param is_hts is the API for HTS indicators (HTS_TST or HTS_TST_POS), default = FALSE
#' @param baseurl API base url, default = https://final.datim.org/
#'
#' @export
#'
#' @examples
#' \dontrun{
#'  #get OU UID
#'   ouuid <- identify_ouuids() %>% dplyr::filter(ou == "Ghana")
#'  #get facility level
#'   faclvl <- identify_levels("Ghana", "facility", username = myuser, password = mypwd())
#'  #gen url
#'   myurl <- gen_url(ouuid, faclvl, org_type = facility) }

gen_url <- function(ou_uid, org_lvl, org_type = "facility",
                    value_type = "results", is_hts = FALSE,
                    baseurl = "https://final.datim.org/"){

  fy_pd <- paste0(curr_fy-1, "Oct")

  core_url <-
    paste0(baseurl,"api/29/analytics?",
           "dimension=pe:", fy_pd, "&", #period
           "dimension=ou:LEVEL-", org_lvl, ";", ou_uid, "&", #level and ou
           "dimension=bw8KHXzxd9i:NLV6dy7BE2O&", #Funding Agency -> USAID
           "dimension=SH885jaRe0o&", #Funding Mechanism
           "dimension=xRo1uG2KJHk&", #Age: <15/15+ (Coarse)
           "dimension=jyUTj5YC3OK&", #Cascade sex
           "dimension=IeMmjHyBUpi:", #Targets / Results ->
           ifelse(value_type == "results", "Jh0jDM5yQ2E", "W8imnja2Owd"))  # targets = W8imnja2Owd, results = Jh0jDM5yQ2E

  if(is_hts == TRUE){
    tech_url <-
      paste0(core_url,
             "dimension=LxhLO68FcXm:f5IPTM7mieH;wdoUps1qb3V;BTIqHnjeG7l;rI3JlpiuwEK;CUblPgOMGaT&", #technical area
             "dimension=ra9ZqrTtSQn&", #HTS Modality (USE ONLY for FY20,21 Results/FY21,22 Targets)
             "dimension=bDWsPYyXgWP:awSDzziN3Dn;EvyNJHbQ7ZE;mSBg9AZx1lV;viYXyEy7wKi&") #HIV Test Status (Specific)) - Pos/Neg + New Pos/Neg
  } else {
    tech_url <-
      paste0(core_url,
             # "dimension=LxhLO68FcXm:", ifelse(org_type == "community", "gma5vVZgK49","udCop657yzi;MvszPTQrUhy;gma5vVZgK49;wdoUps1qb3V"), "&", #technical areas, prep targets at community
             "dimension=LxhLO68FcXm:udCop657yzi;MvszPTQrUhy;gma5vVZgK49;wdoUps1qb3V&", #technical areas
             "dimension=HWPJnUTMjEq:Qbz6SrpmJ1y;h0pvSVe1TYf;pxz2gGSIQhG&") #Disaggregation Type -> Age/Sex, Age/Sex/HIVStatus, Age Aggregated/Sex/HIVStatus
  }

  # if(org_type == "community")
  #   tech_url <-
  #   paste0(tech_url,
  #          "dimension=mINJi7rR1a6:PvuaP6YALSA;AookYR4ECPH&") #Type of organisational unit -> Community & Other organisation unit type

  final_url <-
    paste0(tech_url,
           "displayProperty=SHORTNAME&skipMeta=false&hierarchyMeta=true")

  return(final_url)

}


# Extract All Targets -----------------------------------------------------

#' Extract DATIM Results and Targets (DATIM API Call)
#'
#' @param ou_name Operating Unit name, if mechanism is not specified
#' @param username DATIM username
#' @param password DATIM password, recommend using `mypwd()`
#' @param baseurl API base url, default = https://final.datim.org/
#' @param quarters_complete no. of quarters completed through FY to determine weeks left in year
#' @param folderpath_output folder path to store DATIM output, default = NULL
#'
#' @export
#'
#' @examples
#' \dontrun{
#'  #ou targets
#'  myuser <- "UserX"
#'  mech_x_targets <- pull_mer(ou_name = "Namibia", username = myuser, password = mypwd(myuser))
#'  }

pull_mer <- function(ou_name = NULL,
                     username, password,
                     baseurl = "https://final.datim.org/",
                     quarters_complete = NULL,
                     folderpath_output = NULL){

  print(paste("Extracting targets for", ou_name, format(Sys.time(), "%H:%M:%S")))

  #identify reporting levels
  ou_info <- identify_levels(ou_name, username = username, password = password, baseurl = baseurl) %>%
    dplyr::left_join(identify_ouuids(username = username, password = password, baseurl = baseurl),
                     by = c("country_name" = "displayName"))
  ou_fac <- ou_info$facility
  ou_comm <- ou_info$community
  ou_psnu <- ou_info$prioritization
  ou_uid <- ou_info$id

  #pull non-HTS data (vars only facility)
  df_nonhts <-
    gen_url(ou_uid, ou_fac, type_hts = NULL, baseurl = baseurl) %>%
    get_datim_targets(username, password)

  #remove VMMC_CIRC Age/Sex results since targets and results reported under Age/Sex/HIVStatus
  if(!is.null(df_nonhts))
    df_nonhts <- dplyr::filter(df_nonhts, !(`Technical Area` == "VMMC_CIRC" & `Disaggregation Type` == "Age/Sex"))

  #pull PrEP targets (some at community)
  df_prep_comm_targets <-
    gen_url(ou_uid, ou_comm, org_type = "community", baseurl = baseurl) %>%
    get_datim_targets(username, password)

  #pull HTS data (facility) results
  df_hts_fac_results <-
    gen_url(ou_uid, ou_fac, type_hts = "results", baseurl = baseurl) %>%
    get_datim_targets(username, password)

  #pull HTS data (facility) targets
  df_hts_fac_targets <-
    gen_url(ou_uid, ou_fac, type_hts = "targets", baseurl = baseurl) %>%
    get_datim_targets(username, password)

  #pull HTS data (community) results
  df_hts_comm_results <-
    gen_url(ou_uid, ou_comm, org_type = "community", type_hts = "results", baseurl = baseurl) %>%
    get_datim_targets(username, password)

  #add community level if same as psnu, otherwise will be missing
  if(!is.null(df_hts_comm_results) && ou_psnu == ou_comm)
    df_hts_comm_results <- dplyr::mutate(df_hts_comm_results, !!paste0("orglvl_", ou_psnu) := `Organisation unit`)

  #pull HTS data (community) targets
  df_hts_comm_targets <-
    gen_url(ou_uid, ou_comm, org_type = "community", type_hts = "targets", baseurl = baseurl) %>%
    get_datim_targets(username, password)

  #add community level if same as psnu, otherwise will be missing
  if(!is.null(df_hts_comm_targets) && ou_psnu == ou_comm)
    df_hts_comm_targets <- dplyr::mutate(df_hts_comm_targets, !!paste0("orglvl_", ou_psnu) := `Organisation unit`)

  #ensure data exists before continuing
  data_exists <- (max(nrow(df_hts_comm_results), nrow(df_hts_comm_targets),
                      nrow(df_hts_fac_results), nrow(df_hts_fac_targets),
                      nrow(df_nonhts), nrow(df_prep_comm_targets), 1, na.rm = TRUE) - 1) > 0

  data_exists_hts <- (max(nrow(df_hts_comm_results), nrow(df_hts_comm_targets),
                          nrow(df_hts_fac_results), nrow(df_hts_fac_targets), 1, na.rm = TRUE) - 1) > 0

  if(data_exists){

    #combine all HTS data
    df_combo_hts <- dplyr::bind_rows(df_hts_fac_results, df_hts_fac_targets,
                                     df_hts_comm_results, df_hts_comm_targets)

    #remove extra status (known pos, recent negatives, unknown status) & unify technical area
      df_combo_hts <- df_combo_hts %>%
        dplyr::filter(!`HIV Test Status (Specific)` %in%
                        c("Known at Entry Positive (Specific)",
                          "Recent Negatives (Specific)",
                          "HIV Status Unknown (Specific)")) %>%
        dplyr::mutate(`Technical Area` = "HTS_TST")

    #create HTS_TST_POS
      df_hts_pos <- df_combo_hts %>%
        dplyr::filter(`HIV Test Status (Specific)` %in% c("HIV Positive (Specific)",
                                                          "Newly Identified Positive (Specific)")) %>%
        dplyr::mutate(`Technical Area` = "HTS_TST_POS")

    #bind and aggregate HTS and HTS_POS
      grp_keep <- names(df_combo_hts) %>%
        dplyr::setdiff(c("HTS Modality (USE ONLY for FY19 Results/FY20 Targets)",
                         "HTS Modality (USE ONLY for FY20 Results/FY21 Targets)",
                         "HIV Test Status (Specific)",
                         "Type of organisational unit",
                         "Value"))
    if(data_exists_hts){
      df_combo_hts <- df_combo_hts %>%
        dplyr::bind_rows(df_hts_pos) %>%
        dplyr::group_by_at(grp_keep) %>%
        dplyr::summarise(Value = sum(Value, na.rm = TRUE)) %>%
        dplyr::ungroup()
    } else {
      df_combo_hts <- NULL
    }


    #combine non HTS and HTS dfs
      df_combo <- dplyr::bind_rows(df_nonhts, df_prep_comm_targets, df_combo_hts)

    #clean up orgunits, keeping just OU, PSNU, Community and Facility
      country_name <- unique(df_combo$orglvl_3)
      if(stringr::str_detect(country_name, "Region"))
        country_name <- unique(df_combo$orglvl_4) %>% setdiff(NA)

      df_combo <- purrr::map_dfr(.x = country_name,
                                 .f = ~ hierarchy_rename(df_combo, .x, username, password, baseurl))

    #clean variables and variable names
      df_combo <- df_combo %>%
        dplyr::rename(fy = Period, mech_name = `Funding Mechanism`, fundingagency = `Funding Agency`,
                      #primepartner = `Implementing Partner`,
                      agecoarse = `Age: <15/15+  (Coarse)`,
                      sex = `Cascade sex`, indicator = `Technical Area`, type = `Targets / Results`) %>%
        dplyr::select(-dplyr::matches("Disaggregation Type", "Type of organisational unit")) %>%
        tibble::add_column(mech_code = as.character(NA), .before = "mech_name") %>%
        tidyr::separate(mech_name, c(NA, "mech_code", "mech_name"), sep = " - ", extra = "merge") %>%
        dplyr::mutate(fy = stringr::str_sub(fy,-4), #%>% as.integer,
                      agecoarse = stringr::str_remove(agecoarse, " \\(Inclusive\\)"),
                      sex = stringr::str_remove(sex, "s$"),
                      type = stringr::str_replace(type, " ", "_") %>% tolower) %>%
        dplyr::group_by_if(is.character) %>%
        dplyr::summarise(Value = sum(Value, na.rm = TRUE)) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(fy = as.integer(fy)) %>%
        tidyr::spread(type, Value, fill = 0)

    #export
      hfr_export(df_combo, folderpath_output, type = "DATIM", by_mech = TRUE, quarters_complete)

    invisible(df_combo)

    } else {
      invisible(NULL)
    }
  }



