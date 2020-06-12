#' Load DATIM Look up tables
#'
#' @param datim_path location of datim look up tables files
#' @export
#' @examples
#' \dontrun{
#'  #load look up data
#'  load_lookups(datim_path = "./Data/datim")}

load_lookups <- function(datim_path = "../datim") {

  #Lookup files
    lfiles <- list.files(path = datim_path,
                         pattern = "^HFR_FY20_GLOBAL",
                         full.names = TRUE)

  #Folder should contain valid files
    if ( !exists("lfiles") | length(lfiles) < 3 ) {
      stop("There are not enough look up files. Make sure you have orglevels, orghierarchy and mechanisms files in your datim folder.")
    }

  #HFR Periods
    valid_dates <<- Wavelength::hfr_identify_pds(fy=2020)

  #Sex
    valid_sex <<- c("Female", "Male", NA)

  #Age
    valid_age <<- c("<15", "15+", NA)

  #ORG Levels
    file_orglevels <- lfiles %>%
      stringr::str_subset("orglevels.*.csv$")

    orglevels <<- readr::read_csv(file = file_orglevels)

  #ORG Hirarchy
    file_orgs <- lfiles %>%
      stringr::str_subset("orghierarchy_.*.csv$")

    orgs <<- readr::read_csv(file = file_orgs)

  #Mechanisms
    file_mechanisms <- lfiles %>%
      stringr::str_subset("mechanisms_.*.csv$")

    ims <<- readr::read_csv(file = file_mechanisms)
}
