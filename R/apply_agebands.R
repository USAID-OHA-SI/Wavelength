#' Add mapped age columns onto dataframe
#'
#' @param df data frame with `agefine` or `agesemifine`
#'
#' @export
#'

apply_agebands <- function(df){

  if("agefine" %in% names(df)){
    df <- dplyr::left_join(df, age_map, by = "agefine")
    #TODO - Arrange age columns together
  } else if("agesemifine" %in% names(df)){
    df <- age_map %>%
      dplyr::filter(!agefine %in% c("05-09", "44-49")) %>%
      dplyr::mutate(agefine = stringr::str_replace(agefine, "01-04|40-44", as.character(NA))) %>%
      dplyr::left_join(df, ., by = "agesemifine")
    #TODO - Arrange age columns together
  }

  return(df)
}
