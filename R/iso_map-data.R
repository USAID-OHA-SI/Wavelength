#' ISO Code Mapping Table
#'
#' Table that cross references the ISO Country codes to the
#' PEPFAR Operating Units and Countries. This table is used
#' during the export process for file naming. ISO codes were
#' originall pulled from DATIM.
#'
#' @docType data
#'
#' @usage data(iso_map)
#' @source \url{"https://final.datim.org/api/dataStore/dataSetAssignments/orgUnitLevels"}
#'
#' @format A data frame with each PEPFAR OU and associated ISO code.
#' \describe{
#'   \item{operatingunit}{PEPFAR Operating Unit or country}
#'   \item{iso}{3 letter ISO code for the Operatingunit or country}
#'   \item{regional}{TRUE if operatingunit is a country under a regional program}
#' }

"iso_map"
