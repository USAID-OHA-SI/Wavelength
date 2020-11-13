#' Title
#'
#' @param filepath path to sitelist file
#' @param folderpath_output if output is desired, full folder path
#'
#' @export
#'
#' @examples
#' \dontrun{
#'   path <- "~/Data/HFR_FY21_SiteValidation_Kenya.xlsx"
#'   df_sites <- tidy_sitelist(path) }

tidy_sitelist <- function(filepath, folderpath_output = NULL){

  # print(filepath)

  #read in Site template
    df <- readxl::read_excel(filepath, sheet = "Site List",
                             col_types = c(.default = "text"))

  #remove ending mechanism rows
    df_keep <- df %>%
      dplyr::filter(stringr::str_detect(mech_partner, "!", negate = TRUE))

  #remove any sites that are missing UIDs
    df_keep <- dplyr::filter(df_keep, !is.na(orgunit))

  #reshape, and only keep reporting indicators
    df_keep <- df_keep %>%
      tidyr::pivot_longer(HTS_TST:VMMC_CIRC,
                          names_to = "indicator",
                          values_to = "expect_reporting") %>%
      dplyr::filter(expect_reporting %in% c("KEEP", "ADD")) %>%
      dplyr::mutate(expect_reporting = TRUE)

  #breakout mech info
    df_keep <- df_keep %>%
      tidyr::separate(mech_partner, c("mech_code", "mech_name", "primepartner"), sep = "([[:digit:]]: | \\[)")

  #create HTS_TST_POS and TX_MMD from HTS_TST and TX_CURR
    df_addtl <- df_keep %>%
      dplyr::filter(indicator %in% c("HTS_TST", "TX_CURR")) %>%
      dplyr::mutate(indicator = dplyr::recode(indicator,
                                              "HTS_TST" = "HTS_TST_POS",
                                              "TX_CURR" = "TX_MMD"))

  #bind df together and arrange for output
  df_sites <- df_keep %>%
    dplyr::bind_rows(df_addtl) %>%
    dplyr::arrange(orgunit, mech_code, indicator)

  if(!is.null(folderpath_output))
    hfr_export(df_sites, folderpath_output, type = "SiteList")

  return(df_sites)

}
