#' Uganda Indicator Mapping Table
#'
#' Table that cross references between partner data
#' collected/reported to the mission monthly
#' and the PEPFAR MER indicators.
#'
#' @docType data
#'
#' @usage data(ind_map_uga)
#'
#' @format A data frame including all relevant indicators
#' \describe{
#'   \item{indicator_orig}{indicator name used by UGA}
#'   \item{disaggregate_orig}{disaggregate name used by UGA}
#'   \item{indicator}{standardized indicator to work across partners and OUs}
#'   \item{disaggregate}{components of the indicator that take direction from the MER}
#'   \item{agecoarse}{age, <15/15+}
#'   \item{agesemifine}{VMMC age bands, <15, 15-29, 30+}
#'   \item{sex}{sex, male, female, or peds}
#'   \item{modality}{testing modalities (MER)}
#' }

"ind_map_uga"
