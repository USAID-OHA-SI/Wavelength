#' Export High Frequency Data
#'
#' @param df structured High Frequency Data Frame
#' @param folderpath_output provide the full path to the folder for saving
#' @param type type of data being saved, default = processed
#' @param by_mech export by mechanism, default = FALSE
#' @param quarters_complete FOR DATIM ONLY: # of quarters completed through FY to determine weeks left in year
#'
#' @export
#' @importFrom stats median
#'
#' @examples
#' \dontrun{
#'  #write output
#'    hfr_export(df_tza, "~/WeeklyData") }

hfr_export <- function(df, folderpath_output = NULL,
                       type = "processed", by_mech = FALSE, quarters_complete = NULL){

  if(!is.null(folderpath_output)){

    #export
      cat("\nExport:\n")

      if(by_mech == TRUE){
        #by mechanism, compile file name  and export data
        purrr::walk(.x = unique(df$mech_code),
                    .f = ~ hfr_export_mech(df, .x, type, folderpath_output))
      } else {
        #get ISO code for file nameing
          if(var_exists(df, "countryname")) {
            ou <- unique(df$countryname)
          } else if(var_exists(df, "operatingunit")) {
            ou <- unique(df$operatingunit)
            ou <- ifelse(stringr::str_detect(ou,"/"), stringr::str_remove(ou, "^.*/"), ou)
          } else {
            ou <- NULL
          }

          if(length(ou) == 1){
            iso <- dplyr::filter(iso_map, operatingunit == ou) %>%
              dplyr::pull(iso)
          } else {
            iso <- "GLOBAL"
          }

        #get date for file naming
          date <- format(Sys.Date(),"%Y%m%d")

        #get period for naming
          if(var_exists(df, "hfr_pd")) {
            pd <- median(df$fy) + median(df$hfr_pd)/100
          } else if (!is.null(quarters_complete)) {
            pd <- paste0("FY",curr_fy()-2000, "Q", quarters_complete)
          } else {
            pd <- paste0("FY",curr_fy()-2000)
          }

        #compile file name  and export data
          filename <- paste("HFR", pd, iso, type, date, sep = "_") %>%
            paste0(".csv") %>%
            stringr::str_replace_all("_{2,}", "_")

          readr::write_csv(df, file.path(folderpath_output, filename), na = "")

          cat(crayon::blue("         ",file.path(filename), "\n"))
      }
  }
}


#' Export csv files by mechanism
#'
#' @param df tructured High Frequency Data Frame
#' @param mech mech_code
#' @param type type type of data being saved, default = processed
#' @param folderpath_output provide the full path to the folder for saving

hfr_export_mech <- function(df, mech, type, folderpath_output){

  #filter to mechanism
    df_mech <- dplyr::filter(df, mech_code == mech)

  #update type to reflect mechs with errors
    if ( var_exists(df_mech, "errors") & TRUE %in% unique(df_mech$errors)) {
      type = "errors"
      df_mech <- df_mech %>% dplyr::select(-errors)
    }

  #get ISO code for file nameing
    if(var_exists(df, "countryname")) {
      ou <- unique(df$countryname)
    } else if(var_exists(df, "operatingunit")) {
      ou <- unique(df$operatingunit)
      ou <- ifelse(stringr::str_detect(ou,"/"), stringr::str_remove(ou, "^.*/"), ou)
    } else {
      ou <- NULL
    }

    if(length(ou) == 1){
      iso <- dplyr::filter(iso_map, operatingunit == ou) %>%
        dplyr::pull(iso)
    } else {
      iso <- "GLOBAL"
    }


  #get date for file naming
    date <- format(Sys.Date(),"%Y%m%d")

  #get period for naming
    if(var_exists(df, "hfr_pd")) {
      pd <- median(df$fy) + median(df$hfr_pd)/100
    } else if (!is.null(quarters_complete)) {
      pd <- paste0("FY",curr_fy()-2000, "Q", quarters_complete)
    } else {
      pd <- paste0("FY",curr_fy()-2000)
    }

  #compile file name
    filename <- paste("HFR", pd, iso, mech, type, date, sep = "_") %>% paste0(".csv") %>% stringr::str_replace_all("_{2,}", "_")

    #export data
    readr::write_csv(df_mech, file.path(folderpath_output, filename), na = "")
    cat(crayon::blue("         ",file.path(filename), "\n"))

}

