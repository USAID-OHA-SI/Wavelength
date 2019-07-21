## DATIM API CALL FOR TARGETS
## Mechanism x Site

#https://www.datim.org/api/dimensions?filter=dimension:like:IeMmjHyBUpi


# Check for Packges that are in Suggests ----------------------------------

#' Check if package exists
#'
#' @param pkg package name
#'
#' @export

  package_check <- function(pkg){
    if (!requireNamespace(pkg, quietly = TRUE)) {
      stop(paste("Package", pkg, "needed for this function to work. Please install it."),
           call. = FALSE)
    }
  }

# Login -------------------------------------------------------------------

#' Proper credentials from secure file
#'
#' @param username DATIM username
#'
#' @export
#'
#' @examples
#' \dontrun{
#'  #see information about keyringr about setting up storing secure credentials
#'   myuser <- "UserX"
#'   mypwd(myuser) }

    mypwd <- function(username) {

      package_check("keyringr")

      credential_label <- username
      credential_path <- paste0(Sys.getenv("USERPROFILE"),
                                '\\DPAPI\\passwords\\', Sys.info()["nodename"],
                                '\\', credential_label, '.txt')
      pass <- keyringr::decrypt_dpapi_pw(credential_path)
      return(pass)
    }



# Identify active mechanisms by OU ----------------------------------------

#' Identify active mechanisms by OU
#'
#' @param curr_fy current fiscal year to filter by, default = 2019
#' @param mechid mechanism id to filter by, optional
#' @param extract variable to extract from the mechanism table, optional
#' @param baseurl API base url
#'
#' @export
#'
#' @examples
#' \dontrun{
#'  #full active mechanism table
#'   identify_mechs()
#'  #mechanism information for a particular mechanism
#'   identify_mechs(mechid = 00001)
#'  #uid for a particular mechanism
#'   identify_mechs(mechid = 00001, extract = "uid") }

    identify_mechs <- function(curr_fy = 2019, mechid = NULL, extract = NULL, baseurl = "https://final.datim.org/"){

      package_check("curl")
      package_check("httr")
      package_check("jsonlite")

      stopifnot(curl::has_internet())

      fy_start <- paste(curr_fy-1, "10-01", sep = "-")

      mechs <-
        paste0(baseurl,"api/sqlViews/fgUtV6e9YIX/data.json") %>%
        httr::GET(httr::timeout(60)) %>%
        httr::content("text") %>%
        jsonlite::fromJSON(flatten=TRUE)

      mechs <- mechs %>%
        purrr::pluck("rows") %>%
        tibble::as_tibble(.name_repair = ~ mechs$headers$column)

      mechs <- mechs %>%
        dplyr::mutate(enddate = lubridate::ymd(enddate)) %>%
        dplyr::filter(agency == "USAID",
                      enddate > fy_start)

      if(!is.null(mechid))
        mechs <- dplyr::filter(mechs, code == mechid)

      if(!is.null(mechid) && !is.null(extract))
        mechs <- dplyr::pull(mechs, !!extract)

      return(mechs)
    }


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
#'  ous <- identify_ouuids() }

  identify_ouuids <- function(username, password, baseurl = "https://final.datim.org/"){

    package_check("httr")
    package_check("jsonlite")

    baseurl %>%
      paste0("api/organisationUnits?filter=level:eq:3") %>%
      httr::GET(httr::authenticate(username,password)) %>%
      httr::content("text") %>%
      jsonlite::fromJSON(flatten=TRUE) %>%
      purrr::pluck("organisationUnits")
  }


# Identify Levels ---------------------------------------------------------

#' Identify Facility/Community levels in org hierarchy
#'
#' @param ou operating unit name
#' @param type extraction type, eg facility, community
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
#'    identify_levels("Kenya", username = myuser, password = mypwd())
#'  #facility level of Kenya
#'    identify_levels("Kenya", "facility", username = myuser, password = mypwd()) }

  identify_levels <- function(ou = NULL, type = NULL, username, password, baseurl = "https://final.datim.org/"){

    package_check("httr")
    package_check("jsonlite")

    df_levels <- baseurl %>%
      paste0(.,"api/dataStore/dataSetAssignments/ous") %>%
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

    if(!is.null(ou) && !is.null(type))
      df_levels <- dplyr::pull(df_levels, type)

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
#'  myurl <- paste0(baseurl,
#'                  "api/29/analytics.json?dimension=LxhLO68FcXm:udCop657yzi&dimension=ou:LEVEL-4;HfVjCurKxh2&filter=pe:2018Oct&displayProperty=SHORTNAME&outputIdScheme=CODE")
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
#' @param mech_uid UID for a specific mechanism, recommend using `identify_mechs()`
#' @param ou_uid UID for the country, recommend using `identify_ouuids()`
#' @param org_lvl org hierarchy level, eg facility is level 7 in country X, recommend using `identify_levels()`
#' @param org_type organization type, either facility (default) or community
#' @param type_hts is the API call for HTS indicators ("results", "targets"), default = NULL
#' @param curr_fy current fiscal year, default = 2019
#' @param baseurl API base url, default = https://final.datim.org/
#'
#' @export
#'
#' @examples
#' \dontrun{
#'  #get mech UID
#'   mechuid <- identify_mechs(mechid = 00001, extract = "uid")
#'  #get OU UID
#'   ouuid <- identify_ouuids() %>% dplyr::filter(ou == "Ghana")
#'  #get facility level
#'   faclvl <- identify_levels("Ghana", "facility", username = myuser, password = mypwd())
#'  #gen url
#'   myurl <- gen_url(mechuid, ouuid, faclvl, org_type = facility, type_hts = NULL) }

  gen_url <- function(mech_uid, ou_uid, org_lvl, org_type = "facility", type_hts = NULL, curr_fy = 2019, baseurl = "https://final.datim.org/"){

    fy_pd <- paste0(curr_fy-1, "Oct")

    core_url <-
      paste0(baseurl,"api/29/analytics?",
             "dimension=pe:", fy_pd, "&", #period
             "dimension=ou:LEVEL-", org_lvl, ";", ou_uid, "&", #level and ou
             #"dimension=SH885jaRe0o:", mech_uid, "&", #Funding Mechanism
             "dimension=bw8KHXzxd9i:NLV6dy7BE2O&", #Funding Agency -> USAID
             "dimension=SH885jaRe0o&", #Funding Mechanism
             "dimension=BOyWrF33hiR&", #Implementing Partner
             "dimension=xRo1uG2KJHk&", #Age: <15/15+ (Coarse)
             "dimension=jyUTj5YC3OK&") #Cascade sex

    if(!is.null(type_hts)){
      tech_url <-
        paste0(core_url,
               "dimension=IeMmjHyBUpi:", ifelse(type_hts == "results", "Jh0jDM5yQ2E", "W8imnja2Owd"), "&", #Targets / Results -> targets = W8imnja2Owd, results = Jh0jDM5yQ2E
               "dimension=LxhLO68FcXm:f5IPTM7mieH;wdoUps1qb3V;BTIqHnjeG7l;rI3JlpiuwEK;CUblPgOMGaT&", #technical area
               "dimension=", ifelse(type_hts == "results", "Jm6OwL9IqEa", "CKTkg8dLlr7"), "&", #HTS Modality (USE ONLY for FY19 Results/FY20 Targets) pr HTS Modality (USE ONLY for FY18 Results/FY19 Targets)
               "dimension=bDWsPYyXgWP:awSDzziN3Dn;EvyNJHbQ7ZE;i4Fgst9vzF9;mSBg9AZx1lV;viYXyEy7wKi;GNXy3pvTTM2&") #HIV Test Status (Specific))
    } else {
      tech_url <-
        paste0(core_url,
               "dimension=IeMmjHyBUpi&", #Targets / Results -> targets W8imnja2Owd
               "dimension=LxhLO68FcXm:", ifelse(org_type == "community", "gma5vVZgK49","udCop657yzi;MvszPTQrUhy;gma5vVZgK49;wdoUps1qb3V"), "&", #technical areas, prep targets at community
               "dimension=HWPJnUTMjEq:Qbz6SrpmJ1y;h0pvSVe1TYf;pxz2gGSIQhG&") #Disaggregation Type -> Age/Sex, Age/Sex/HIVStatus, Age Aggregated/Sex/HIVStatus
    }

    if(org_type == "community")
      tech_url <-
        paste0(tech_url,
               "dimension=mINJi7rR1a6:PvuaP6YALSA&") #Type of organisational unit -> Community

    final_url <-
      paste0(tech_url,
             "displayProperty=SHORTNAME&skipMeta=false&hierarchyMeta=true")

    return(final_url)

  }


# Extract All Targets -----------------------------------------------------

#' Extract Targets (DATIM API Call)
#'
#' @param mechanism_id DATIM mechanism ID
#' @param ou_name Operating Unit name, if mechanism is not specified
#' @param username DATIM username
#' @param password DATIM password, recommend using `mypwd()`
#' @param baseurl API base url, default = https://final.datim.org/
#'
#' @export
#'
#' @examples
#' \dontrun{
#'  #mechanism targets
#'  myuser <- "UserX"
#'  mech_x_targets <- extract_targets(mechanism_id = 00001, username = myuser, password = mypwd(myuser))
#'  #ou targets
#'  mech_x_targets <- extract_targets(ou_name = "Namibia", username = myuser, password = mypwd(myuser))
#'  }

  extract_targets <- function(mechanism_id = NULL,
                              ou_name = NULL,
                              username, password,
                              baseurl = "https://final.datim.org/"){

    #ensure that either ou_name or mechanism_id is entered
      # if(is.null(mechanism_id))
      #   stopifnot(!is.null(ou_name))

    #identify OU and UID from mechanim info
      if(!is.null(mechanism_id)){
          mech_info <- identify_mechs(mechid = mechanism_id, baseurl = baseurl)
          mech_uid <- mech_info$uid
          ou_name <- mech_info$ou
      }

    #identify reporting levels
      ou_info <- identify_levels(ou_name, username = username, password = password, baseurl = baseurl) %>%
        dplyr::left_join(identify_ouuids(username = username, password = password, baseurl = baseurl),
                         by = c("name3" = "displayName"))
      ou_fac <- ou_info$facility
      ou_comm <- ou_info$community
      ou_psnu <- ou_info$prioritization
      ou_uid <- ou_info$id

    #pull non-HTS data (vars only facility)
      df_nonhts <-
        gen_url(mech_uid, ou_uid, ou_fac, type_hts = NULL, baseurl = baseurl) %>%
        get_datim_targets(username, password)
      #remove VMMC_CIRC results w/ Age/Sex/HIVStatus since there is also Age/Sex used for Total Numerator
      if(!is.null(df_nonhts))
        df_nonhts <- dplyr::mutate(df_nonhts, Value = ifelse(`Technical Area` == "VMMC_CIRC" & `Disaggregation Type` == "Age/Sex/HIVStatus" & `Targets / Results` == "MER Results", NA, Value))

    #pull PrEP targets (some at community)
      df_prep_comm_targets <-
        gen_url(mech_uid, ou_uid, ou_comm, org_type = "community", baseurl = baseurl) %>%
        get_datim_targets(username, password)

    #pull HTS data (facility) results
      df_hts_fac_results <-
        gen_url(mech_uid, ou_uid, ou_fac, type_hts = "results", baseurl = baseurl) %>%
        get_datim_targets(username, password)

    #pull HTS data (facility) targets
      df_hts_fac_targets <-
        gen_url(mech_uid, ou_uid, ou_fac, type_hts = "targets", baseurl = baseurl) %>%
        get_datim_targets(username, password)

    #pull HTS data (community) results
      df_hts_comm_results <-
        gen_url(mech_uid, ou_uid, ou_comm, org_type = "community", type_hts = "results", baseurl = baseurl) %>%
        get_datim_targets(username, password)
      #add community level if same as psnu, otherwise will be missing
      if(!is.null(df_hts_comm_results) && ou_psnu == ou_comm)
        df_hts_comm_results <- dplyr::mutate(df_hts_comm_results, !!paste0("orglvl_", ou_psnu) := `Organisation unit`)

    #pull HTS data (community) targets
      df_hts_comm_targets <-
        gen_url(mech_uid, ou_uid, ou_comm, org_type = "community", type_hts = "targets", baseurl = baseurl) %>%
        get_datim_targets(username, password)
      #add community level if same as psnu, otherwise will be missing
      if(!is.null(df_hts_comm_targets) && ou_psnu == ou_comm)
        df_hts_comm_targets <- dplyr::mutate(df_hts_comm_targets, !!paste0("orglvl_", ou_psnu) := `Organisation unit`)

    #ensure data exists before continuing
      data_exists <- (max(nrow(df_hts_comm_results), nrow(df_hts_comm_targets),
                          nrow(df_hts_fac_results), nrow(df_hts_fac_targets),
                          nrow(df_nonhts), nrow(df_prep_comm_targets), 1, na.rm = TRUE) - 1) > 0

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
                         "HTS Modality (USE ONLY for FY18 Results/FY19 Targets)",
                         "HIV Test Status (Specific)",
                         "Type of organisational unit",
                         "Value"))

      df_combo_hts <- df_combo_hts %>%
        dplyr::bind_rows(df_hts_pos) %>%
        dplyr::group_by_at(grp_keep) %>%
        dplyr::summarise(Value = sum(Value, na.rm = TRUE)) %>%
        dplyr::ungroup()

    #combine non HTS and HTS dfs
      df_combo <- dplyr::bind_rows(df_nonhts, df_prep_comm_targets, df_combo_hts)

    #clean up orgunits, keeping just OU, PSNU, Community and Facility
      df_combo <- df_combo %>%
        dplyr::rename(facility = `Organisation unit`,
                      operatingunit = orglvl_3,
                      snu1 = orglvl_4)

      if(!!paste0("orglvl_", ou_psnu) %in% names(df_combo))
        df_combo <- dplyr::rename(df_combo, psnu = !!paste0("orglvl_", ou_psnu))

      if(ou_psnu == ou_comm && !!paste0("orglvl_", ou_comm) %in% names(df_combo)){
        df_combo <- df_combo %>%
          tibble::add_column(community = as.character(NA), .after = "psnu") %>%
          dplyr::mutate(community = psnu)
      } else if (!!paste0("orglvl_", ou_comm) %in% names(df_combo)){
        df_combo <- dplyr::rename(df_combo, community = !!paste0("orglvl_", ou_comm))
      }

      df_combo <- df_combo %>%
        dplyr::select(facility, orgunituid, dplyr::everything()) %>%
        dplyr::select(-dplyr::starts_with("orglvl_"))

    #clean variables and variable names
      df_combo <- df_combo %>%
        dplyr::rename(fy = Period, implementingmechanismname = `Funding Mechanism`, fundingagency = `Funding Agency`,
                      primepartner = `Implementing Partner`, agecoarse = `Age: <15/15+  (Coarse)`,
                      sex = `Cascade sex`, indicator = `Technical Area`, type = `Targets / Results`) %>%
        dplyr::select(-dplyr::matches("Disaggregation Type")) %>%
        tibble::add_column(mechanismid = as.character(NA), .before = "implementingmechanismname") %>%
        dplyr::mutate(fy = stringr::str_sub(fy,-4), #%>% as.integer,
                      mechanismid = stringr::str_extract(implementingmechanismname, "^[:alnum:]{5,6}"),
                      implementingmechanismname = stringr::str_remove(implementingmechanismname, "^[:alnum:]{5,6} - "),
                      agecoarse = stringr::str_remove(agecoarse, " \\(Inclusive\\)"),
                      sex = stringr::str_remove(sex, "s$"),
                      type = stringr::str_replace(type, " ", "_") %>% tolower) %>%
        dplyr::group_by_if(is.character) %>%
        dplyr::summarise(Value = sum(Value, na.rm = TRUE)) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(fy = as.integer(fy)) %>%
        tidyr::spread(type, Value, fill = 0)

    return(df_combo)

    } else {
      return(NULL)
    }
  }


# Adjust targets ----------------------------------------------------------

#' Adjust Annual Targets to work with HFR peirods
#'
#' @param df data frame created by `extract_targets()`
#' @param quarters_complete MER quarters with data available
#'
#' @export
#'
#' @examples
#' \dontrun{
#'  myuser <- "UserX"
#'  mech_x_targets <- extract_targets(00001, myuser, mypwd(myuser))
#'  mech_x_targets <- periodize_targets(mech_x_targets, 2) }

periodize_targets <- function(df, quarters_complete){

  weeks_remaining <- 52 - (quarters_complete * 13)

   df %>%
    #dplyr::group_by(fy, orgunituid) %>%
    dplyr::mutate(weekly_targets = mer_targets / 52,
                  targets_gap = ifelse((mer_targets - mer_results) < 0,0, mer_targets - mer_results),
                  weekly_targets_gap = targets_gap/weeks_remaining)
    #dplyr::ungroup()
}
