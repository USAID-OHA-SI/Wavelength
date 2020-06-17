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
    if ( !exists("lfiles") | length(lfiles) < 2 ) {
      stop("There are not enough look up files [HFR_FY20_GLOBAL...]. Make sure you have orghierarchy and mechanisms files in your datim folder.")
    }

  #HFR Periods
    valid_dates <<- Wavelength::hfr_identify_pds(fy=2020)

  #Sex
    valid_sex <<- c("Female", "Male", NA)

  #Age
    valid_age <<- c("<15", "15+", NA)


  #ORG Hirarchy
    file_orgs <- lfiles %>%
      stringr::str_subset("._orghierarchy_\\d{8}.csv$")

    orgs <<- readr::read_csv(file = file_orgs)

    if (is.null(orgs) | !is.data.frame(orgs)) {
      stop("Orghierarchy file does not seems to have valid content")
    }

  #Mechanisms
    file_mechanisms <- lfiles %>%
      stringr::str_subset("._mechanisms_\\d{8}.csv$")

    ims <<- readr::read_csv(file = file_mechanisms)

    if (is.null(orgs) | !is.data.frame(orgs)) {
      stop("Mechanisms file does not seems to have valid content")
    }
}
