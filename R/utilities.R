#' Fiscal Year
#'
#' @return Current Fiscal Year
#'
  curr_fy <- function() { 2020 }



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
#'
#' @examples
#' \dontrun{
#'  #see information about keyringr about setting up storing secure credentials
#'   myuser <- "UserX"
#'   mypwd(myuser) }

  mypwd <- function(username) {

    package_check("keyringr")

    credential_label <- username
    credential_path <- paste0(Sys.getenv("USERPROFILE"),
                              '\\DPAPI\\passwords\\', Sys.info()["nodename"],
                              '\\', credential_label, '.txt')
    pass <- keyringr::decrypt_dpapi_pw(credential_path)
    return(pass)
  }




#' Check if variable exist
#'
#' @param df data frame to check against
#' @param var quoted variable of interest

  var_exists <- function(df, var) {

    var %in% names(df)

  }


#' Determine whether meta tab exists
#'
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
                                              "Template|HFR FY and|, eg 2020.1|perating|nit| ")
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
#'
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

