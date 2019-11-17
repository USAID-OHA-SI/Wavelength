#' Determine whether meta tab exists
#'
#' @param filepath filepath to sumbitted template
#'
#' @export

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
#' @param type type of meta data requesting: ou, period, version, type (default)
#'
#' @export
#'
#' @examples
#' \dontrun{
#' #identify whether template is long or wide
#'   filepath <- "~/WeeklyData/Raw/KEN_Weekly.xlsx"
#'   extract_meta(filepath, meta_type = "type")
#' #identify period
#'   extract_meta(filepath, meta_type = "period")
#' #identify OU
#'   extract_meta(filepath, meta_type = "period") }

extract_meta <- function(filepath, meta_type = "type"){

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


# #check structure
# sht <- "HFR"
# headers <- readxl::read_excel(filepath, sheet = sht, skip = 1,
#                               col_types = "text", n_max = 0) %>% names()


#check for orgunituids
#check for OU
#check for mech_code
#check for indicators
  # c("HTS_TST", "HTS_TST_POS", "PrEP_NEW", "TX_CURR", "TX_MMD", "TX_NEW", "VMMC_CIRC"))
#check age/sex/otherdisagg
#check dates
