#' Arrange variable order
#'
#' @param df data frame after restructuring
#'
#' @export

order_vars <- function(df) {

  #full set of ordered variables
    sel_vars <- c("operatingunit",
                  "fundingagency",
                  "date",
                  "fy",
                  "snu1",
                  "psnu",
                  "community",
                  "facility",
                  "orgunituid",
                  "partner",
                  "mechanismid",
                  "reporting_freq",
                  "indicator",
                  "disaggregate",
                  "agecoarse",
                  "agesemifine",
                  "sex",
                  "modality",
                  "otherdisaggregate",
                  "val")

  #identify missing variables to add
    add <- sel_vars[!sel_vars%in%names(df)]

  #add missing variables
    if(length(add)!=0) df[add] <- NA

  #arrange variables
   df_clean <- dplyr::select_at(df, sel_vars)

   dropped_vars <- paste(setdiff(names(df), names(df_clean)), collapse = ", ")
   if(length(dropped_vars)>0)
     print(paste0("WARNING: The following variables were dropped: ", dropped_vars))

  invisible(df_clean)
}
