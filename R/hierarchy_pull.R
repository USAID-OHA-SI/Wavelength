#' Pull Hierarchy Data from DATIM
#'
#' @param ou_uid UID for the country, recommend using `identify_ouuids()`
#' @param username DATIM username
#' @param password DATIM password, recommend using `mypwd()`
#' @param baseurl API base url, default = https://final.datim.org/
#'
#' @export
#'
#' @examples
#' \dontrun{
#' #get OU UID
#'   ouuid <- identify_ouuids() %>% dplyr::filter(ou == "Kenya")
#' #pull hierarchy (paths are all UIDs)
#'   df <- hierarchy_pull(ouuid, username = myuser, password = mypwd(myuser)) }

hierarchy_pull <- function(ou_uid, username, password, baseurl = "https://final.datim.org/"){
  #compile url
  url <- paste0(baseurl,
                "api/organisationUnits?filter=path:like:", ou_uid,
                "&fields=id,name,path,level,coordinates&paging=false")

  #pull data from DATIM
  df <- url %>%
    httr::GET(httr::authenticate(username,password)) %>%
    httr::content("text") %>%
    jsonlite::fromJSON() %>%
    purrr::pluck("organisationUnits") %>%
    tibble::as_tibble()
}



#' Clean up DATIM Hierarchy Path
#'
#' @param df data frame created by `hierarchy_pull()`
#'
#' @export

hierarchy_clean <- function(df){

  #create header for each level of the org hierarchy
  levels <- df$path %>%
    stringr::str_count("/") %>%
    max()

  headers <- paste0("orglvl_", seq(1:levels))

  #separate out path into each level of the org hierarchy (UIDs)
  df <- df %>%
    dplyr::mutate(path = stringr::str_remove(path, "^/")) %>%
    tidyr::separate(path, headers, sep = "/", fill = "right") %>%
    dplyr::select(-orglvl_1, -orglvl_2)

  #convert level of the org hierarchy from UIDs to names
  df_key <- dplyr::select(df, name, id)

  df <- df %>%
    dplyr::mutate_at(dplyr::vars(dplyr::starts_with("orglvl")),
                     ~ plyr::mapvalues(., df_key$id, df_key$name, warn_missing = FALSE))
  return(df)
}

#' Rename Hierarchy from Levels to OU/SNU1/PSNU/Facility
#'
#' @param df data frame created by `hierarchy_pull() %>% hierarchy_clean()`
#' @param country county name, eg "Malawi" or "Nepal"
#' @param username DATIM username
#' @param password DATIM password, recommend using `mypwd()`
#' @param baseurl API base url, default = https://final.datim.org/
#'
#' @export

hierarchy_rename <- function(df, country, username, password, baseurl = "https://final.datim.org/"){

  if(!country %in% unique(df$orglvl_3))
    df <- dplyr::filter(df, orglvl_4 == country)

  df_ou_info <- identify_levels(country, username = username, password = password, baseurl = baseurl)

  if(NROW(df_ou_info) > 0 && df_ou_info$facility > 0){
    #identify levels
    ou_country <- df_ou_info$country
    ou_psnu <- df_ou_info$prioritization
    ou_fac <- df_ou_info$facility
    ou_comm <- df_ou_info$community


    #clean up orgunits, keeping just OU, PSNU, Community and Facility
    df <- df %>%
      dplyr::rename(orgunit = name,
                    operatingunit = orglvl_3,
                    snu1 = orglvl_4,
                    orgunituid = id)

    if(!!paste0("orglvl_", ou_country) %in% names(df)) {
      df <- df %>%
        tibble::add_column(countryname = NA, .after = "operatingunit") %>%
        dplyr::mutate(df, country = !!paste0("orglvl_", ou_country))
    }

    if("orglvl_4" %in% names(df)) {
      df <- df %>%
        tibble::add_column(snu1 = NA, .after = "country") %>%
        dplyr::mutate(df, snu1 = orglvl_4)
    }

    if(!!paste0("orglvl_", ou_psnu) %in% names(df))
      df <- dplyr::rename(df, psnu = !!paste0("orglvl_", ou_psnu))

    if(ou_psnu == ou_comm && !!!paste0("orglvl_", ou_comm) %in% names(df)){
      df <- df %>%
        tibble::add_column(community = as.character(NA), .after = "psnu") %>%
        dplyr::mutate(community = psnu)
    } else if (!!paste0("orglvl_", ou_comm) %in% names(df)){
      df <- dplyr::rename(df, community = !!paste0("orglvl_", ou_comm))
    }

    if(!!paste0("orglvl_", ou_fac) %in% names(df))
      df <- dplyr::rename(df, facility = !!paste0("orglvl_", ou_fac))

    df <- df %>%
      dplyr::select(orgunit, orgunituid, dplyr::everything()) %>%
      dplyr::select(-dplyr::starts_with("orglvl_"))

    return(df)
  } else {
    return(NULL)
  }

}


#' Compile PEPFAR Hierarchy
#'
#' @param ou_uid UID for the country, recommend using `identify_ouuids()`
#' @param username DATIM username
#' @param password DATIM password, recommend using `mypwd()`
#' @param baseurl API base url, default = https://final.datim.org/
#'
#' @export
#'
#' @examples
#' \dontrun{
#' #get OU UID
#'   ouuid <- identify_ouuids() %>% dplyr::filter(ou == "Kenya")
#' #pull hierarchy (paths are all UIDs)
#'   df <- hierarchy_compile(ouuid, username = myuser, password = mypwd(myuser)) }

hierarchy_compile <- function(ou_uid, username, password, baseurl = "https://final.datim.org/"){

  print(ou_uid)

  df <- hierarchy_pull(ou_uid, username, password, baseurl)

  df <- hierarchy_clean(df)

  ou_name <- unique(df$orglvl_3)
  country_name <- dplyr::case_when(stringr::str_detect(ou_name, "Region") ~ unique(df$orglvl_4) %>% setdiff(NA),
                                   TRUE ~ ou_name)

  df <- purrr::map_dfr(.x = country_name,
                       .f = ~ hierarchy_rename(df, .x, username, password, baseurl))

  return(df)
}
