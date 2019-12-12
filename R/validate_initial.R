#' Initial Validation on Submitted template
#'
#' @param filepath filepath to sumbitted template
#'
#' @export

validate_initial <- function(filepath){

  #initial validation before proceeding to see if any HFR named tabs
  is_hfrtab(filepath)

  check_meta(filepath)
  check_tabs(filepath)

}



#' Determine if there are tabs to import
#'
#' @param filepath filepath to sumbitted template
#'

is_hfrtab <- function(filepath){

  if(missing(filepath))
    stop("No filepath provided.")

  shts <- readxl::excel_sheets(filepath)

  shts_hfr <- shts %>%
    stringr::str_subset("HFR") %>%
    length()

  msg <- paste("No HFR tabs included in file: ", basename(filepath))


  if(shts_hfr == 0)
    stop(msg)

}



#' Inspect Meta data
#'
#' @param filepath filepath to sumbitted template

check_meta <- function(filepath){
  #type
    if(is_metatab(filepath)){
      type <- hfr_extract_meta(filepath, "type")
      temp_version <- hfr_extract_meta(filepath, "version")
    } else {
      df <- hfr_import(filepath)
      type <- ifelse(var_exists(df, "val"), "Long [no meta provided]", "Wide [no meta provided]")
      temp_version <- "[no meta provided]"
    }

    temp_version <- crayon::blue(temp_version)

  #country/file
    ou_name <-  hfr_extract_meta(filepath, "ou")
    ou_name <- ifelse(is.na(ou_name), crayon::yellow(ou_name), crayon::blue(ou_name))
    file_name <- crayon::blue(basename(filepath))

  #PRINT VALIDATION

  cat("\n--------------------------------------------",
      "\nCountry:", ou_name,
      "\nFile name:", file_name,
      "\nWhat template was submitted?", temp_version
      )
}


#' Validate Submission's tabs for import
#'
#' @param filepath filepath to sumbitted template

check_tabs <- function(filepath){

  #tabs
    tabs <- readxl::excel_sheets(filepath)

    tabs_imported <- tabs %>%
      stringr::str_subset("HFR") %>%
      paste(collapse = ",")

    tabs_imported_ok <- ifelse(length(tabs_imported) > 0, crayon::green("TRUE"), crayon::green("FALSE"))

    tabs_imported <- crayon::blue(tabs_imported)

    tabs_excluded <- tabs %>%
      stringr::str_subset("HFR", negate = TRUE) %>%
      paste(collapse = ",") %>%
      crayon::yellow()

  #PRINT VALIDATION

    cat("\nAre there tabs to import [must be labeled 'HFR']?", tabs_imported_ok,
        "\nWhat tabs will be imported?", tabs_imported,
        "\nWhat tabs will be excluded?", tabs_excluded)
}
