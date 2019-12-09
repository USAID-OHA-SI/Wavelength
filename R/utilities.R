#' Fiscal Year
#'
#' @export
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
