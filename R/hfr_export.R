#' Export High Frequency Data
#'
#' @param df structured High Frequency Data Frame
#' @param folderpath_output provide the full path to the folder for saving
#' @param type type of data being saved, default = processed
#' @param quarters_complete FOR DATIM ONLY: # of quarters completed through FY to determine weeks left in year
#'
#' @export
#'
#' @examples
#' \dontrun{
#'  #write output
#'    hfr_export(df_tza, "~/WeeklyData") }

hfr_export <- function(df, folderpath_output = NULL, type = "processed", quarters_complete = NULL){

  if(!is.null(folderpath_output)){

    #get ISO code for file nameing
    if(var_exists(df, "countryname")) {
      ou <- unique(df$countryname)
    } else if(var_exists(df, "operatingunit")) {
      ou <- unique(df$operatingunit)
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
      filename <- paste("HFR", pd, iso, type, date, sep = "_") %>% paste0(".csv") %>% stringr::str_remove_all("__")

    #export data
      readr::write_csv(df, file.path(folderpath_output, filename), na = "")
  }
}


