#' ISO Code Mapping Table
#'
#' Table that cross references the ISO Country codes to the
#' PEPFAR Operating Units and Countries. This table is used
#' during the export process for file naming. ISO codes were
#' originall pulled from Wikipedia \url
#' {https://en.wikipedia.org/wiki/List_of_ISO_3166_country_codes}
#' and adapted for regional programs.
#'
#' @docType data
#'
#' @usage data(iso_map)
#' @source \url{https://github.com/ICPI/DIV/blob/master/Reference_Materials/2017.12.15_PEPFAR_ISOcodes.csv}
#'
#' @format A data frame with each PEPFAR OU and associated ISO code.
#' \describe{
#'   \item{operatingunit}{PEPFAR Operating Unit}
#'   \item{countryname}{country under PEPFAR Operating Unit for regional programs}
#'   \item{iso_ou}{3 letter ISO code for the Operating Unit}
#'   \item{iso}{3 letter ISO code for the country}
#' }

"iso_map"
