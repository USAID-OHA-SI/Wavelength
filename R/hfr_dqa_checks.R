#' Validate reporting dates
#'
#' @param .data df
#' @param df_dates df_dates
#' @return boolean
#' @export
#' @examples
#' \dontrun{
#'   data %>% is_date_valid(df_dates)
#' }
#'
is_date_valid <- function(.data, df_dates) {

  df_dates <- df_dates %>%
    dplyr::mutate(
      fy = as.integer(fy),
      hfr_pd = as.integer(hfr_pd),
      valid_date = TRUE
    )

  .data %>%
    dplyr::left_join(df_dates, by=c("date", "fy", "hfr_pd")) %>%
    dplyr::mutate(valid_date = ifelse(is.na(valid_date), FALSE, valid_date))
}


#' Check if OperatingUnit is valid
#'
#' @param data df
#' @param df_orgs df_orgs
#' @return boolean
#' @export
#' @examples
#' \dontrun{
#'   data %>% is_ou_valid(df_orgs)
#' }
#'
is_ou_valid <- function(.data, df_orgs) {

  orgs <- df_orgs %>%
    dplyr::select(operatingunit) %>%
    dplyr::distinct() %>%
    dplyr::mutate(valid_ou = TRUE)

  if("valid_ou" %in% names(.data)) {
    .data <- .data %>% dplyr::select(-valid_ou)
  }

  .data %>%
    dplyr::left_join(orgs, by = "operatingunit") %>%
    dplyr::mutate(valid_ou = ifelse(is.na(valid_ou), FALSE, valid_ou))
}


#' Check if Operating Unit name is valid
#'
#' @param data df
#' @param df_orgs orgs
#' @return boolean
#' @export
#' @examples
#' \dontrun{
#'   data %>% is_orgunituid_valid(df_orgs)
#' }
#'
is_orgunituid_valid <- function(.data, df_orgs) {

  orgs <- df_orgs %>%
    dplyr::select(orgunituid) %>%
    dplyr::distinct() %>%
    dplyr::mutate(valid_uid = TRUE)

  .data %>%
    dplyr::mutate(orgunituid = stringr::str_remove_all(stringr::str_trim(orgunituid, side="both"), pattern = "\r\n")) %>%
    dplyr::left_join(orgs, by = "orgunituid") %>%
    dplyr::mutate(valid_uid = ifelse(is.na(valid_uid), FALSE, valid_uid))
}


#' Check if orgunituid exist in operating unit
#'
#' @param data df
#' @param df_orgs org_hierarchy df
#' @return data with new column: T/F
#' @export
#' @examples
#' \dontrun{
#'   data %>% orgunituid4ou(df_orgs)
#' }
#'
is_orgunituid4ou <- function(.data, df_orgs) {

  orgs <- df_orgs %>%
    dplyr::select(operatingunit, orgunituid) %>%
    dplyr::mutate(uid_to_ou = TRUE)

  .data %>%
    dplyr::left_join(orgs, by=c("operatingunit", "orgunituid")) %>%
    dplyr::mutate(uid_to_ou = ifelse(valid_ou == FALSE | is.na(uid_to_ou), FALSE, uid_to_ou))
}

#' Validate mechanism code
#'
#' @param .data df
#' @param df_mechs mechs
#' @return boolean
#' @export
#' @examples
#' \dontrun{
#'   data %>% is_mech_valid(df_dates)
#' }
is_mech_valid <- function(.data, df_mechs) {

  mechs <- df_mechs %>%
    dplyr::select(mech_code) %>%
    dplyr::distinct() %>%
    dplyr::mutate(
      mech_code = as.integer(mech_code),
      valid_mech = TRUE
    )

  .data %>%
    dplyr::mutate(mech_code = as.integer(mech_code)) %>%
    dplyr::left_join(mechs, by = "mech_code") %>%
    dplyr::mutate(valid_mech = ifelse(is.na(valid_mech), FALSE, valid_mech))
}


#' Validate mechanism code
#'
#' @param .data df
#' @param df_mechs mechs
#' @return boolean
#' @export
#' @examples
#' \dontrun{
#'   data %>% is_mech4ou_valid(df_mechs)
#' }
is_mech4ou <- function(.data, df_mechs) {

  mechs <- df_mechs %>%
    dplyr::select(operatingunit, mech_code) %>%
    dplyr::distinct() %>%
    dplyr::mutate(
      mech_code = as.integer(mech_code),
      mech_to_ou = TRUE
    )

  .data %>%
    dplyr::mutate(mech_code = as.integer(mech_code)) %>%
    dplyr::left_join(mechs, by = c("operatingunit", "mech_code")) %>%
    dplyr::mutate(mech_to_ou = ifelse(is.na(mech_to_ou), FALSE, mech_to_ou))
}


