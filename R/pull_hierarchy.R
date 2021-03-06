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
#'   df <- hierarchy_extract(ouuid, username = myuser, password = mypwd(myuser)) }

hierarchy_extract <- function(ou_uid, username, password, baseurl = "https://final.datim.org/"){

  package_check("curl")
  package_check("httr")
  package_check("jsonlite")

  stopifnot(curl::has_internet())

  #compile url
  url <- paste0(baseurl,
                "api/organisationUnits?filter=path:like:", ou_uid,
                "&fields=id,name,path,level,geometry&paging=false")

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
#' @param df data frame created by `hierarchy_extract()`
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

  #store uids in order to keep the psnuuid in hierarchy_rename()
    df_uids <- df %>%
      dplyr::select(name, id, dplyr::starts_with("orglvl")) %>%
      dplyr::rename_with(~ stringr::str_replace(., "org","uid"))

  #convert level names of the org hierarchy from UIDs to names
    df_key <- dplyr::select(df, name, id)
    df <- df %>%
      dplyr::mutate_at(dplyr::vars(dplyr::starts_with("orglvl")),
                       ~ plyr::mapvalues(., df_key$id, df_key$name, warn_missing = FALSE))

  #add uids back on
    df <- dplyr::left_join(df, df_uids, by = c("name", "id"))

  #clean up coordinates, removing polygons and separating into lat-long
    if(var_exists(df, "geometry")) {

      #extract coordinates from nested df
        df <- df %>%
          dplyr::mutate(geom_type = geometry$type,
                        coordinates = geometry$coordinates) %>%
          dplyr::select(-geometry)

      #for point data, unnest coordinate from list
        sites <- df %>%
          dplyr::mutate(coordinates = dplyr::na_if(coordinates, "NULL")) %>%
          dplyr::filter((geom_type == "Point" | is.na(geom_type)) & !is.na(coordinates))  %>%
          dplyr::select(-geom_type) %>%
          tidyr::unnest_wider(coordinates, names_sep = "_")

        if(nrow(sites) > 0){
          sites <- sites %>%
            dplyr::rename(longitude = "coordinates_1", latitude = "coordinates_2") %>%
            dplyr::mutate_at(dplyr::vars("longitude", "latitude"), as.double) %>%
            dplyr::select(id, latitude, longitude)
          #bind coordinates onto hierarchy table
          df <- dplyr::left_join(df, sites, by = "id")
        }

      #remove unnecessary columns
        df <- df %>%
          dplyr::select(-geom_type, -coordinates)
    }

  return(df)
}

#' Rename Hierarchy from Levels to OU/SNU1/PSNU/Facility
#'
#' @param df data frame created by `hierarchy_extract() %>% hierarchy_clean()`
#' @param country county name, eg "Malawi" or "Nepal"
#' @param username DATIM username
#' @param password DATIM password, recommend using `mypwd()`
#' @param baseurl API base url, default = https://final.datim.org/
#'
#' @export

hierarchy_rename <- function(df, country, username, password, baseurl = "https://final.datim.org/"){

  package_check("curl")
  package_check("httr")
  package_check("jsonlite")

  stopifnot(curl::has_internet())

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
      dplyr::rename_at(dplyr::vars(dplyr::matches("name|Organisation unit")), ~ "orgunit")

    if(var_exists(df, "id"))
      df <- dplyr::rename(df, orgunituid = id)

    if(ou_country == 3) {
      df <- df %>%
        dplyr::mutate(countryname = orglvl_3) %>%
        dplyr::relocate(countryname, .after = "orglvl_3")
    } else {
      df <- df %>%
        dplyr::mutate(countryname = orglvl_4) %>%
        dplyr::relocate(countryname, .after = "orglvl_3")
    }

    if("orglvl_4" %in% names(df)) {
      df <- df %>%
        dplyr::mutate(snu1 = orglvl_4) %>%
        dplyr::relocate(snu1, .after = "countryname")
    }

    if(!!paste0("orglvl_", ou_psnu) %in% names(df))
      df <- dplyr::rename(df, psnu = !!paste0("orglvl_", ou_psnu))

    if(ou_psnu == ou_comm && !!!paste0("orglvl_", ou_comm) %in% names(df)){
      df <- df %>%
        dplyr::mutate(community = psnu) %>%
        dplyr::relocate(community, .after = "psnu")
    } else if (!!paste0("orglvl_", ou_comm) %in% names(df)){
      df <- dplyr::rename(df, community = !!paste0("orglvl_", ou_comm))
    }

    if(!!paste0("orglvl_", ou_fac) %in% names(df))
      df <- dplyr::rename(df, facility = !!paste0("orglvl_", ou_fac))

    df <- dplyr::rename(df, operatingunit = orglvl_3)

    #add in psnu uid
    if(!!paste0("uidlvl_", ou_psnu) %in% names(df)){
      df <- df %>%
        dplyr::rename(psnuuid = !!paste0("uidlvl_", ou_psnu)) %>%
        dplyr::relocate(psnuuid, .after = "psnu")
    }

    #reorder and remove unused vars
    df <- df %>%
      dplyr::select(orgunit, orgunituid, dplyr::everything()) %>%
      dplyr::select(-dplyr::starts_with("orglvl_"), -dplyr::starts_with("uidlvl_"))


    return(df)
  } else {
    return(NULL)
  }

}



#' Extract country name from OU or country name
#'
#' @param df data frame created by `hierarchy_extract() %>% hierarchy_clean()`
#'
#' @export

hierarchy_identify_ctry <- function(df){

  #pull orglvl_3 which is out
    country_name <- unique(df$orglvl_3)

  #for regional missions, need to pull country name from orglvl_4
    if(stringr::str_detect(country_name, "Region"))
      country_name <- unique(df$orglvl_4) %>% setdiff(NA)

  return(country_name)
}


#' Compile PEPFAR Hierarchy
#'
#' @param ou_uid UID for the country, recommend using `identify_ouuids()`
#' @param username DATIM username
#' @param password DATIM password, recommend using `mypwd()`
#' @param baseurl API base url, default = https://final.datim.org/
#' @param folderpath_output provide the full path to the folder for saving
#'
#' @export
#'
#' @examples
#' \dontrun{
#' #get OU UID
#'   ouuid <- identify_ouuids() %>% dplyr::filter(ou == "Kenya")
#' #pull hierarchy (paths are all UIDs)
#'   df <- pull_hierarchy(ouuid, username = myuser, password = mypwd(myuser)) }

pull_hierarchy <- function(ou_uid, username, password, baseurl = "https://final.datim.org/", folderpath_output = NULL){

  print(ou_uid)

  df <- hierarchy_extract(ou_uid, username, password, baseurl)

  df <- hierarchy_clean(df)

  country_name <- hierarchy_identify_ctry(df)

  df <- purrr::map_dfr(.x = country_name,
                       .f = ~ hierarchy_rename(df, .x, username, password, baseurl))

  hfr_export(df, folderpath_output, type = "mechanisms")

  return(df)
}
