#' Joing HFR and DATIM Data
#'
#' @param path_hfr file path to HFR processed data
#' @param path_datim file path to DATIM extract, see `extract_datim()`
#' @param start_date start date of HFR period
#' @param hfr_pd HFR period # (for saving)
#' @param weeks number of weeks to create, default = 4
#' @param output_folder folder path for saving the output
#'
#' @export
#'
#' @examples
#' \dontrun{
#' #identify file paths
#'   path_datim <- list.files("out/DATIM", "FY19Q2_Zam", full.names = TRUE) %>% fs::path_abs()
#'   path_hfr <- list.files("out/processed", "ZMB", full.names = TRUE) %>% fs::path_abs()
#' #combine
#'   combine_sources(path_hfr, path_datim, "2019-06-10", 10, output_folder = "out/joint") }

combine_sources <- function(path_hfr, path_datim,
                            start_date, hfr_pd,
                            weeks = 4,
                            output_folder = "out/joint"){

  #import
    if(tools::file_ext(path_hfr) == "txt"){
      df_hfr <- readr::read_tsv(path_hfr, col_types = c(.default = "c")) %>%
        dplyr::mutate(date = lubridate::ymd(date))
    } else {
      df_hfr <- readr::read_csv(path_hfr, col_types = c(.default = "c")) %>%
        dplyr::mutate(date = lubridate::mdy(date))
    }

    if("site" %in% names(df_hfr)) df_hfr <- dplyr::rename(df_hfr, facility = site)

    df_mer <- readr::read_tsv(path_datim, col_types = c(.default = "c"))

  #rename org/partner info (HFR)
  # vars_dup <- c("operatingunit", "fundingagency", "snu1", "psnu", "facility") #"partner"
  # if("community" %in% names(df_hfr)) vars_dup <- c(vars_dup, "community")
  # vars_rename <- c("partner")

  # df_hfr <- df_hfr %>%
  #   dplyr::select(-vars_dup) %>%
  #   dplyr::rename_at(dplyr::vars(vars_rename), ~ paste0(., "_hfr"))

  #duplicate targets for each week (DATIM)
    dates <- lubridate::as_date(start_date) %>% seq(by = 7, length.out = weeks)
    df_mer_rpt <- purrr::map_dfr(.x = dates,
                                 .f = ~dplyr::mutate(df_mer, date = .x))

  #join
    df_join <- dplyr::full_join(df_mer_rpt, df_hfr) # , by = c("orgunituid", "fy", "mechanismid", "agecoarse", "sex", "indicator", "date"))

  #export
    ou <- df_join$operatingunit[1]
    if(stringr::str_detect(ou, "Reg")) ou <- df_join$snu1[1]
    iso <- dplyr::filter(iso_map, countryname == ou) %>%
      dplyr::pull(iso)

    filename <- paste0("HFR_JOINT_", iso, "_FY19P", hfr_pd, ".txt") %>% file.path(output_folder, .)
    readr::write_tsv(df_join, filename, na = "")

}
