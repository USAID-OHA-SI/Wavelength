#' Read in HFR output file
#'
#' @param filepath filepath of an HFR output file
#'
#' @export
#'
#' @examples
#' \dontrun{
#'  path <- "~/data/HFR_2020.01_Global_output_20191204.1705.csv"
#'  df <- hfr_read(path)
#'  }

hfr_read <- function(filepath){
  #import
    df <- vroom::vroom(filepath, delim = ",", col_types = c(.default = "c"))

  #conver date to date format
    df <- dplyr::mutate(df, date = lubridate::as_date(date))

  #convert year and period to integer
    df <- dplyr::mutate_at(df, dplyr::vars(fy, hfr_pd), as.integer)

  #covert other numeric variables to double
    df <- dplyr::mutate_at(df, dplyr::vars(dplyr::starts_with("val"),
                                           dplyr::starts_with("mer"),
                                           dplyr::starts_with("weekly"),
                                           dplyr::starts_with("targets")), as.double)

  #FY21 expect reporting var to logical
    if("expect_reporting" %in% names(df))
      df <- dplyr::mutate(df, expect_reporting = as.logical(expect_reporting))

  #convert blanks to NAs
    df <- dplyr::mutate_if(df, is.character, ~ dplyr::na_if(., "(NA|)"))
}

