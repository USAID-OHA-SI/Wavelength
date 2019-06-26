#' Export High Frequency Data
#'
#' @param df structured High Frequency Data Frame
#' @param folderpath_output provide the full path to the folder for saving
#'
#' @export
#'
#' @examples
#' \dontrun{
#'  #write output
#'    export_hfr(df_tza, "TZA", "~/WeeklyData") }

export_hfr <- function(df, folderpath_output = NULL){

  if(!is.null(folderpath_output)){

    #get ISO code for file nameing
      ou <- unique(df$operatingunit)
      iso_ou <- dplyr::filter(iso_map, operatingunit == ou) %>%
        dplyr::pull(iso_ou)

    #get date for file naming
      date <- format(Sys.Date(),"%Y%m%d")

    #compile file name
      filename <- paste0("HFR_", iso_ou, "_", date, ".txt")

    #export data
      readr::write_tsv(df, file.path(folderpath_output, filename), na = "")
  }
}


