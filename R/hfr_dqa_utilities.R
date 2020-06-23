#' Get operating unit name
#'
#' @param iso_code iso3 code
#' @param orglevels df org levels
#' @return operating unit
#' @export
#' @examples
#' \dontrun{
#'   get_operatingunit(org_levels, 'XWA')
#' }
#'
get_operatingunit <- function(iso_code, orglevels)  {

  iso = iso_code %>% toupper()

  if (iso == 'MULTIPLE') {
    return(iso)
  }

  orglevels <- orglevels %>%
    dplyr::filter(iso3 == iso | iso4 == iso)

  if (nrow(orglevels) > 0) {
    return( orglevels$name3[1] )
  }
  else {
    return(NA)
  }
}


#' Get operating unit name
#'
#' @param pfile processed file
#' @param levels org levels
#' @param ims mechanisms df
#' @return operating unit name
#' @export
#' @examples
#' \dontrun{
#'   get_mech_ou(ims, 'HFR_2020.99_XAR_100000_processed_20200528.csv')
#' }
#'
guess_operatingunit <- function(pfile, levels, ims) {

  fcomponents <- validate_submission(pfile, levels, ims)

  #c(
  #   "fy",
  #   "hfr_pd",
  #   "iso3",
  #   "operatingunit",
  #   "mech_code",
  #   "mech_valid",
  #   "mech_name",
  #   "pdate",
  #   "name"
  #)

  ou <- ifelse(fcomponents[6] == 1, fcomponents[4], NA)

  return(ou)
}

#' Extract OU ISO3 code
#'
#' @param pfile processed file
#' @return ISO3 3 character iso code
#' @export
#' @examples
#' \dontrun{
#'   extract_iso3code('HFR_2020.99_XAR_100000_processed_20200528.csv')
#' }
#'
extract_iso3code <- function(pfile) {

  fcomponents <- parse_submission(pfile)

  fsections <- c("hfr_pd", "iso3", "mech_code", "pdate")

  iso <- ifelse(length(fcomponents) == length(fsections), toupper(fcomponents[2]), NA)

  if ( !is.na(iso) & nchar(iso) != 3 ) {
    return(NA)
  }

  return(iso)
}


#' Extract mechanism code
#'
#' @param pfile processed file
#' @return mech_code mechanism code
#' @export
#' @examples
#' \dontrun{
#'   extract_mechcode('HFR_2020.99_XAR_100000_processed_20200528.csv')
#' }
#'
extract_mechcode <- function(pfile) {

  fcomponents <- parse_submission(pfile)

  fsections <- c("hfr_pd","iso3","mech_code","pdate")

  mech_code <- ifelse(
    length(fcomponents) == length(fsections),
    as.integer(fcomponents[5]),
    NA
  )

  return(mech_code)
}


#' Validate mechanism code
#'
#' @param mechanisms df of mechs
#' @param mech_code mech code
#' @return vector c(valid_im,  mech_name)
#' @export
#' @examples
#' \dontrun{
#'   validate_mechanism(ims, 'Angola', 16172)
#' }
#'
validate_mechanism <- function(mechanisms, ou, mcode) {

  uniq_ims <- mechanisms %>%
    dplyr::distinct(mech_code) %>%
    dplyr::pull()

  valid_im <- ifelse(mcode %in% uniq_ims, 1, 0)

  mechanisms <- mechanisms %>%
    dplyr::filter(operatingunit == ou, mech_code == mcode)

  if ( valid_im == 1 & nrow(mechanisms) > 0 ) {
    return( c(valid_im, mechanisms$mech_name[mechanisms$mech_code == mcode]) )
  }
  else{
    return(c(valid_im, NA))
  }
}


#' Parse out submitted file components
#'
#' @param sfile processed file
#' @return component As vector c("hfr_pd", "iso3", "mech_code", "pdate")
#' @export
#' @examples
#' \dontrun{
#'   parse_submission("HFR_2020.99_XWH_100000_processed_20200101.csv")
#' }
#'
parse_submission <- function(pfile) {

  parts <- pfile %>% stringr::str_split("_") %>% unlist()

  parts <- parts[2:length(parts)]
  parts <- parts[-(length(parts)-1)]

  parts[length(parts)] <- stringr::str_remove(parts[length(parts)], ".csv")

  return(parts)
}



#' Validate submitted files
#'
#' @param sfile processed file
#' @param levels org levels
#' @param ims mechanisms df
#' @return vector c("fy", "hfr_pd", "iso3", "operatingunit", "mech_code", "mech_valid", "mech_name", "pdate", "name")
#' @export
#' @examples
#' \dontrun{
#'   validate_submissions("HFR_2020.99_XWH_100000_processed_20200101.csv")
#' }
#'
validate_submission <- function(pfile, levels, ims) {

  # c("hfr_pd", "iso3", "mech_code", "pdate")
  parts <- parse_submission(pfile)

  pd <- parts[1] %>% stringr::str_split("\\D") %>% unlist()

  fy = pd[1]
  pd = pd[2]

  iso <- parts[2]

  ou <- get_operatingunit(iso_code=iso, orglevels=levels)

  mech_code <- parts[3]

  im <- validate_mechanism(mechanisms=ims, ou=ou, mcode=mech_code)

  mech_valid <- im[1]
  mech_name <- im[2]

  pdate <- parts[length(parts)] %>% stringr::str_split(".csv") %>% unlist()
  pdate <- pdate[1]

  parts <- c(fy, pd, iso, ou, mech_code, mech_valid, mech_name, pdate[1], pfile)

  return(parts)
}


#' Validate files
#'
#' @param folder pfolder
#' @param levels org levels
#' @param ims mechanisms df
#' @param pattern filename pattern
#' @return dataframe
#' @export
#' @examples
#' \dontrun{
#'   validate_submissions(dir_hfr_pd205, pattern = "HFR_2020.05")
#' }
#'
validate_submissions <- function(pfolder, levels, ims, pattern=NULL) {

  fcomponents <- c("fy", "hfr_pd", "iso3", "operatingunit", "mech_code", "mech_valid", "mech_name", "pdate", "name")

  if ( is.null(pattern) ) {
    files <- list.files(path = pfolder, full.names = F)
  } else {
    files <- list.files(path = pfolder, pattern = pattern, full.names = F)
  }

  processed <- files %>%
    purrr::map(validate_submission, levels, ims) %>%
    unlist() %>%
    matrix(ncol=9, byrow=T) %>%
    as.data.frame(stringsAsFactors=F) %>%
    purrr::set_names(fcomponents)

  return(processed)
}


#' Report files validation
#'
#' @param df_files df of filename validation
#' @param mechanisms mechs
#' @return void
#' @export
#' @examples
#' \dontrun{
#'   report_file_errors(files, ims, processed)
#' }
#'
report_submissions_errors <- function(df_files, mechanisms, export = FALSE) {

  cat(crayon::blue("Reporting errors from file names ...\n"))

  # All errors
  file_errors <- df_files %>%
    dplyr::filter(is.na(operatingunit) | mech_valid == 0 | is.na(mech_name))

  file_errors %>% glimpse()

  # ISO Errors
  iso_errors <- file_errors %>%
    dplyr::filter(is.na(operatingunit))

  iso_errors_n <- nrow(iso_errors)

  cat(crayon::red(paste0("Invalid ISO Codes: ", iso_errors_n, "\n")))

  if ( iso_errors_n > 0 ) {
    cat(crayon::blue(unique(iso_errors %>% dplyr::pull(iso3)), "\n"))
  }

  # IM errors
  im_errors <- df_files %>%
    dplyr::filter(mech_valid == 0)

  im_errors_n <- nrow(im_errors)

  cat(crayon::red("Invalid IMs: ", im_errors_n, "\n"))

  if ( im_errors_n > 0 ) {

    im_msgs <- im_errors %>%
      dplyr::select(iso3, mech_code) %>%
      mutate(error = paste0(as.character(mech_code), " [", iso3, "]")) %>%
      dplyr::pull(error)

    cat(crayon::blue(unique(im_msgs), "\n"))
  }

  # Unmatched IMs
  im_unmatched <- df_files %>%
    dplyr::filter(is.na(mech_name))

  im_unmatched_n <- nrow(im_unmatched)

  cat(crayon::red("Unmatched IMs: ", im_unmatched_n, "\n"))

  if ( im_unmatched_n > 0 ) {

    im_u_msgs <- im_unmatched %>%
      dplyr::select(iso3, mech_code) %>%
      mutate(error = paste0(as.character(mech_code), " [", iso3, "]")) %>%
      dplyr::pull(error)

    cat(crayon::blue(unique(im_u_msgs), "\n"))
  }

  # Duplicate files

  file_dup <- df_files %>%
    dplyr::group_by(fy, hfr_pd, iso3, mech_code) %>%
    dplyr::summarise(n=n()) %>%
    dplyr::ungroup() %>%
    dplyr::filter(n > 1)

  file_dup_n <- nrow(file_dup)

  cat(crayon::red("Duplucate file: ", file_dup_n, "\n"))

  if ( file_dup_n > 0) {
    file_dup %>%
      dplyr::arrange(iso3, mech_code, desc(n)) %>%
      print()
  }

  # Export
  if (export == TRUE) {
    file_errors %>%
      readr::write_csv(path = here::here(paste0("HFR_", file_errors$fy[1], "_", file_errors$hfr_pd[1], "_filename_errors.csv")))
  }
}


#' Validate HFR PD Date
#'
#' @param df_pds hfr period dates
#' @param pdate period date
#' @param pd reporting period
#' @return valid: True / False
#' @export
#' @examples
#' \dontrun{
#'   validate_date(df_pds = valid_dates, '2020-01-27', 5)
#' }
#'
validate_date <- function(df_pds, pdate, pd) {

  valid <- df_pds %>%
    dplyr::filter(date == pdate, hfr_pd == pd) %>%
    dplyr::count() %>%
    dplyr::pull()

  valid <- ifelse(length(valid) > 0, 1, 0)

  return(valid)
}


#' Validate org unit uid
#'
#' @param df_orgs org hierarchy
#' @param ou operating unit
#' @param uid orgunituid
#' @return valid as a vector c(valid_uid, valid_uid_ou)
#' @export
#' @examples
#' \dontrun{
#'   validate_orgunit(df_orgs, 'Eswatini', 'g48XD8px8NN')
#' }

validate_orgunit <- function(df_orgs, ou, uid) {

  # Is orgunituid valid?
  valid_uid <- ifelse(
    uid %in% (df_orgs %>% dplyr::distinct(orgunituid) %>% dplyr::pull()),
    1,
    0
  )

  # Does uid belong to OU
  valid_uid_ou <- df_orgs %>%
    dplyr::distinct(operatingunit, orgunituid) %>%
    dplyr::filter(
      operatingunit == ou,
      orgunituid == uid
    ) %>%
    dplyr::count() %>%
    dplyr::pull()

  valid_uid_ou <- ifelse(length(valid_uid_ou) > 0, 1, 0)

  # Return results
  valid <- c(valid_uid, valid_uid_ou)

  return(valid)
}


#' Validate processed hfr data
#'
#' @param hfr_data processed hfr data
#' @param levels datim org levels
#' @param orgs datim org hierarchy
#' @param ims datim mechanisms
#' @param dates hfr valid dates
#' @return errors data frame
#' @export
#' @examples
#' \dontrun{
#'   validate_hfr_data(hfr_df, levels=org_levels, orgs=org_hierarchy, ims=mechanis, dates=hfr_dates)
#' }

validate_hfr_data <- function(hfr_data, levels, orgs, ims, dates){

  valid_age <- c("<15", "15+")
  valid_sex <- c("Female", "Male")

  errors <- hfr_data %>%
    group_by(source) %>%
    mutate(row_id = row_number()) %>%
    ungroup() %>%
    is_date_valid(df_dates = valid_dates) %>%
    is_ou_valid(df_orgs = orgs) %>%
    is_ou_valid(df_orgs = orgs) %>%
    is_orgunituid_valid(df_orgs = orgs) %>%
    is_orgunituid4ou(df_orgs = orgs) %>%
    is_mech_valid(df_mechs = ims) %>%
    is_mech4ou(df_mechs = ims) %>%
    mutate(
      valid_age = ifelse(is.na(agecoarse) | agecoarse %in% valid_age, TRUE, FALSE),
      valid_sex = ifelse(is.na(sex) | sex %in% valid_sex, TRUE, FALSE),
      valid_value = ifelse(is.na(val) | val >= 0, TRUE, FALSE)
    ) %>%
    select(source:valid_value) %>%
    distinct() %>%
    mutate(errors = rowSums(.[3:11] == FALSE)) %>%
    filter(errors > 0)

  return(errors)
}


#' Update invalid operating units
#'
#' @param hfr_data processed hfr data
#' @param levels datim org levels
#' @param orgs datim org hierarchy
#' @param ims datim mechanisms
#' @return hfr_data df
#' @export
#' @examples
#' \dontrun{
#'   update_operatingunits(hfr_df, levels=org_levels, orgs=org_hierarchy, ims=mechanisms)
#' }

update_operatingunits <- function(hfr_data, levels, orgs, ims=NULL){

  if( is.null(ims) ){

    hfr_data <- hfr_data %>%
      is_ou_valid(df_orgs = orgs) %>%
      dplyr::rowwise() %>%
      dplyr::mutate(operatingunit = ifelse(valid_ou == FALSE, get_operatingunit(iso_code=extract_iso3code(source), orglevels=levels), operatingunit)) %>%
      dplyr::ungroup() %>%
      dplyr::select(-valid_ou)

  }else{

    hfr_data <- hfr_data %>%
      is_ou_valid(df_orgs = orgs) %>%
      dplyr::rowwise() %>%
      dplyr::mutate(operatingunit = ifelse(valid_ou == FALSE, guess_operatingunit(pfile=source, levels=levels, ims=ims), operatingunit)) %>%
      dplyr::ungroup() %>%
      dplyr::select(-valid_ou)
  }

  return(hfr_data)
}

#' Confirm validations of processed files
#'
#' @param hfr_data content of processed files
#' @param hfr_errors errors detected from files content
#' @param dir_files location of processed files
#' @export
#' @examples
#' \dontrun{
#' confirm_validations(hfr_data, hfr_errors, dir_files)
#' }
#'
confirm_validations <- function(hfr_data, hfr_errors, dir_files){

  hfr_data %>%
    dplyr::left_join(errors, by = "source") %>%
    dplyr::filter(is.na(row_id)) %>%
    dplyr::distinct(source, row_id) %>%
    dplyr::pull(source) %>%
    purrr::map(function(source) {

      valid_file <- stringr::str_replace(source, "processed", "validated")

      file.rename(from = here::here(dir_files, source),
                  to = here::here(dir_files, valid_file))
    })
}


#' Revert validated file to processed files
#'
#' @param dir_files location of processed files
#' @export
#' @examples
#' \dontrun{
#' revert_validations(dir_files)
#' }
#'
revert_validations <- function(dir_files) {

  file_pattern = "HFR_*.*.validated.*.csv$"

  files <- list.files(path=dir_files, pattern = file_pattern, full.names = FALSE)

  if (length(files) == 0) {
    stop(paste0(dir_files, " contains no file that matches this pattern: ", file_pattern))
  }

  files %>%
    purrr::map(function(file) {

      revert_file <- file %>% stringr::str_replace("validated", "processed")

      file.rename(from = here::here(dir_files, file),
                  to = here::here(dir_files, revert_file))
    })
}

