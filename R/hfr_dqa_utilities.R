#' Get operating unit name
#'
#' @param orglevels df org levels
#' @param iso_code iso3 code
#' @return operating unit
#' @export
#' @example
#' \dontrun{
#'   get_operatingunit(org_levels, 'XWA')
#' }
#'
get_operatingunit <- function(orglevels, iso_code) {

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
#' @param levels org levels
#' @param ims mechanisms df
#' @param pfile processed file
#' @return operating unit name
#' @export
#' @example
#' \dontrun{
#'   get_mech_ou(ims, 'HFR_2020.99_XAR_100000_processed_20200528.csv')
#' }
#'
guess_operatingunit <- function(levels, ims, pfile) {

  fcomponents <- validate_submission(levels, ims, pfile)
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


#' Validate mechanism code
#'
#' @param mechanisms df of mechs
#' @param mech_code mech code
#' @return vector c(valid_im,  mech_name)
#' @export
#' @example
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


#' Validate submitted files
#'
#' @param levels org levels
#' @param ims mechanisms df
#' @param sfile processed file
#' @return vector c("fy", "hfr_pd", "iso3", "operatingunit", "mech_code", "mech_valid", "mech_name", "pdate", "name")
#' @export
#' @example
#' \dontrun{
#'   validate_submissions("HFR_2020.99_XWH_100000_processed_20200101.csv")
#' }
#'
validate_submission <- function(levels, ims, pfile) {

  parts <- pfile %>% str_split("_") %>% unlist()
  parts <- parts[2:length(parts)]

  pd <- parts[1] %>% str_split("\\D") %>% unlist()

  fy = pd[1]
  pd = pd[2]

  iso <- parts[2]

  ou <- get_operatingunit(orglevels=levels, iso_code=iso)

  mech_code <- parts[3]

  im <- validate_mechanism(mechanisms=ims, ou=ou, mcode=mech_code)

  mech_valid <- im[1]
  mech_name <- im[2]

  pdate <- parts[length(parts)] %>% str_split(".csv") %>% unlist()
  pdate <- pdate[1]

  parts <- c(fy, pd, iso, ou, mech_code, mech_valid, mech_name, pdate[1], pfile)

  return(parts)
}


#' Validate files
#'
#' @param levels org levels
#' @param ims mechanisms df
#' @param folder pfolder
#' @return dataframe
#' @export
#' @example
#' \dontrun{
#'   validate_submissions(dir_hfr_pd205, pattern = "HFR_2020.05")
#' }
#'
validate_submissions <- function(levels, ims, pfolder, pattern=NULL) {

  fcomponents <- c("fy", "hfr_pd", "iso3", "operatingunit", "mech_code", "mech_valid", "mech_name", "pdate", "name")

  if ( is.null(pattern) ) {
    files <- list.files(path = pfolder, full.names = F)
  }
  else {
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
#' @param mechanisms mechs df
#' @param df_files df of filename validation
#' @return void
#' @export
#' @example
#' \dontrun{
#'   report_file_errors(ims, processed)
#' }
#'
report_submissions_errors <- function(mechanisms, df_files, export = FALSE) {

  cat(crayon::blue("Reporting errors from file names ...\n"))

  # All errors
  file_errors <- df_files %>%
    dplyr::filter(is.na(operatingunit) | mech_valid == 0 | is.na(mech_name))

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
    cat(crayon::blue(unique(im_errors %>% dplyr::pull(mech_code)), "\n"))
  }

  # Unmatched IMs
  im_unmatched <- df_files %>%
    dplyr::filter(is.na(mech_name))

  im_unmatched_n <- nrow(im_unmatched)

  cat(crayon::red("Unmatched IMs: ", im_unmatched_n, "\n"))

  if ( im_unmatched_n > 0 ) {
    cat(crayon::blue(unique(im_unmatched %>% dplyr::pull(mech_code)), "\n"))
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
#' @example
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
#' @example
#' \dontrun{
#'   validate_orgunit(df_orgs, 'Eswatini', 'g48XD8px8NN')
#' }
#'
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
