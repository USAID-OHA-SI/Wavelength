#' Load DATIM Look up tables
#'
#' @param datim_path HFR Data folder ID
#' @param local Read data from local directory?
#' @param user User email address
#' @export
#' @examples
#' \dontrun{
#'  #load look up data
#'  load_lookups(datim_path = "./Data/datim")
#'  load_lookups(datim_path = "999#1aw####")
#'  }

load_lookups <- function(datim_path = "./Data/",
                         local = TRUE,
                         user = NULL) {

  # HFR Periods
  valid_dates <<- Wavelength::hfr_identify_pds(fy = 2021)

  # Sex
  valid_sex <<- c("Female", "Male", NA)

  # Age
  valid_age <<- c("<15", "15+", NA)

  # Read from local directory
  if (local == TRUE) {

    dir <- {{datim_path}}

    cat("\nReading data from local directories", dir, "\n")

    #Lookup files
    lfiles <- list.files(path = dir,
                         pattern = "^HFR_FY21_GLOBAL",
                         full.names = TRUE)

    #Folder should contain valid files
    if ( !exists("lfiles") | length(lfiles) < 2 ) {
      stop("There are not enough look up files [HFR_FY20_GLOBAL...].
           Make sure you have orghierarchy and mechanisms files
           in your datim folder.")
    }

    # ORG Hirarchy
    orgs <<- lfiles %>%
      stringr::str_subset(string = .,
                          pattern = "._orghierarchy_\\d{8}.csv$") %>%
      vroom::vroom()

    # Mechanisms
    ims <<- lfiles %>%
      stringr::str_subset(string = .,
                          pattern = "._mechanisms_\\d{8}.csv$") %>%
      vroom::vroom()

    if (is.null(orgs) | is.null(ims)) {
      cat("Orgs or Mechanisms file does not seems to have valid content")
      return(FALSE)
    }

    return(TRUE)
  }

  # Read data from google drive
  drive_id <- {{datim_path}}

  cat("\nReading data from local directories", drive_id, "\n")

  # Check access
  if (!local & is.null(user) &
      (!googledrive::drive_has_token() |
       !googlesheets4::gs4_has_token())) {

    stop("You need to be authenticated. Set the user parameter.")
  }

  # authenticate
  if (!local & !is.null(user)) {
    googledrive::drive_auth(email = user)
    googlesheets4::gs4_auth(email = user)
  }

  # List out datim data folder
  dirs_lst <- googledrive::drive_ls(
      path = googledrive::as_id(datim_path), # data folder
      pattern = "DATIM Data"
    )

  # Identify Orgs folder id
  df_orgs <- dirs_lst %>%
    dplyr::filter(name == "3.2 DATIM Data (Org Hierarchy)") %>%
    dplyr::pull(id) %>%   #==> Folder id
    googledrive::as_id() %>%
    googledrive::drive_ls(
      path = .,
      pattern = "^HFR_FY20_GLOBAL_orghierarchy_\\d{8}$"
    ) %>%
    dplyr::pull(id) %>%   #=> File id
    googledrive::as_id() %>%
    googlesheets4::read_sheet(ss = ., sheet = 1) #File content

  # Assign to global scope
  orgs <<- df_orgs

  # Mechs
  df_mechs <- dirs_lst %>%
    dplyr::filter(name == "3.3 DATIM Data (Mechanisms)") %>%
    dplyr::pull(id) %>%   #==> Folder id
    googledrive::as_id() %>%
    googledrive::drive_ls(
      path = .,
      pattern = "^HFR_FY20_GLOBAL_mechanisms_\\d{8}$"
    ) %>%
    dplyr::pull(id) %>%   #==> File id
    googledrive::as_id() %>%
    googlesheets4::read_sheet(ss = ., sheet = 1)

  # Assign to global scope
  ims <<- df_mechs

  if (is.null(orgs) | is.null(ims) ) {
    cat("Mechanisms file does not seems to have valid content")
    return(FALSE)
  }

  return(TRUE)
}
