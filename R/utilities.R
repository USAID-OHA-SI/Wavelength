#' Fiscal Year
#'
#' @export
#' @return Current Fiscal Year
#'
  curr_fy <- 2021



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




#' Proper credentials from secure file
#'
#' @param username DATIM username
#'
#' @export
#' @return see keyringr vignette about setting up storing secure credentials
#'
#' @examples
#' \dontrun{
#'   myuser <- "UserX"
#'   mypwd(myuser) }

  mypwd <- function(username) {

    package_check("keyringr")

    credential_label <- username

    #OS specific options
    if(.Platform$OS.type == "windows"){
      credential_path <- paste0(Sys.getenv("USERPROFILE"),
                                '\\DPAPI\\passwords\\', Sys.info()["nodename"],
                                '\\', credential_label, '.txt')
      pass <- keyringr::decrypt_dpapi_pw(credential_path)
    } else if(.Platform$OS.type == "unix") {
      pass <- keyringr::decrypt_kc_pw(credential_label)
    } else {
      stop("Not a PC or MacOS!")
    }

    return(pass)
  }




#' Check if variable exist
#'
#' @export
#' @param df data frame to check against
#' @param var quoted variable of interest

  var_exists <- function(df, var) {

    var %in% names(df)

  }


#' Determine whether meta tab exists
#'
#' @export
#' @param filepath filepath to sumbitted template

is_metatab <- function(filepath){

  if(missing(filepath))
    stop("No filepath provided.")

  shts <- readxl::excel_sheets(filepath)
  "meta" %in% shts
}



#' Extract Meta Data Information about Template
#'
#' @description Useful for pulling information about the template, whether
#' It be the Operating Unit (OU), Period, template version, or type, eg wide or long.
#'
#' @param filepath filepath to sumbitted template
#' @param meta_type type of meta data requesting: ou, period, version, type (default)
#'
#' @export
#'
#' @examples
#' \dontrun{
#' #identify whether template is long or wide
#'   filepath <- "~/WeeklyData/Raw/KEN_Weekly.xlsx"
#'   hfr_extract_meta(filepath, meta_type = "type")
#' #identify period
#'   hfr_extract_meta(filepath, meta_type = "period")
#' #identify OU
#'   hfr_extract_meta(filepath, meta_type = "ou") }

hfr_extract_meta <- function(filepath, meta_type = "type"){

  if(is_metatab(filepath)){
    metatable <- readxl::read_excel(filepath, range = "meta!B2:C5",
                                    col_names = c("mtype", "mvalue"))

    meta <- metatable %>%
      dplyr::mutate(mtype =
                      stringr::str_remove_all(mtype,
                                              "Template|HFR FY and|, eg 2020.1|perating|nit|\\/Country| ")
                    %>% tolower) %>%
      dplyr::filter(mtype == meta_type) %>%
      dplyr::pull()


  } else {
    meta <- NA
  }

  return(meta)
}




#' Count missing values
#'
#' @export
#' @param df data frame
#' @param var variable to count missing values

count_missing <- function(df, var){

  missing <- df %>%
    dplyr::filter(is.na({{var}})) %>%
    NROW()

  missing_pct <- round(missing/NROW(df), 2)*100
  missing_pct <- paste0("(",missing_pct, "%)")

  count <- ifelse(missing > 0, crayon::red(missing, "out of", NROW(df), "rows", missing_pct), crayon::green("No"))
  return(count)
}


#' Flag Missing Variables
#'
#' @export
#'
#' @param required list of required vars
#' @param submitted list of vars pulled from submission
#'
  flag_missing <- function(required, submitted){

    missing <- setdiff(required, submitted)
    if(length(missing) > 0){
      missing <- crayon::yellow(missing)
    } else {
      missing <- crayon::green("No")
    }

    return(missing)
  }


#' Flag Extra Variables
#' @export
#' @param required list of required vars
#' @param submitted list of vars pulled from submission

  flag_extra <- function(required, submitted){

    extra <- setdiff(submitted, required)
    if(length(extra > 0)){
      extra <- crayon::red(extra)
    } else {
      extra <- crayon::green("No")
    }

    return(extra)
  }


#' Paint console text in red
#'
#' @param txt text to be printed
#' @export
#'
paint_red <- function(txt) {
  msg <- crayon::red(txt)
  return(msg)
}

#' Paint console text in green
#'
#' @param txt text to be printed
#' @export
#'
paint_green <- function(txt) {
  msg <- crayon::green(txt)
  return(msg)
}

#' Paint console text in blue
#'
#' @param txt text to be printed
#' @export
#'
paint_blue <- function(txt) {
  msg <- crayon::blue(txt)
  return(msg)
}

#' Paint console text in yellow
#'
#' @param txt text to be printed
#' @export
#'
paint_yellow <- function(txt) {
  msg <- rayon::yellow(txt)
  return(msg)
}



#' Search Org Hierarchy for Org Unit
#'
#' @param df org hierarchy, created in pull_hierarchy()
#' @param orgunit_name full or partial orgunit name for matching
#' @param ou operating unit; if added searches only that OU default = NULL
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # orgunit - Kewot
#' org <- pull_hierarchy(myusername, mypass)
#' hfr_orgunit_search(org, "Kew", "Ethiopia") }

hfr_orgunit_search <- function(df, orgunit_name, ou = NULL){

  if(!is.null(ou))
    df <- dplyr::filter(df, operatingunit == ou)

  df %>%
    dplyr::filter(stringr::str_detect(orgunit, orgunit_name)) %>%
    dplyr::select(orgunit, orgunituid, psnu, operatingunit, level)
}
