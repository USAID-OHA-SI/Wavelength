#' Burundi Indicator Mapping Table
#'
#' Table that cross references between partner data
#' collected/reported to the mission monthly
#' and the PEPFAR MER indicators.
#'
#' @docType data
#'
#' @usage data(ind_map_bdi)
#'
#' @format A data frame including all relevant indicators
#' \describe{
#'   \item{ind}{indicator & disaggregate name used by BDI}
#'   \item{indicator}{standardized indicator to work across partners and OUs}
#'   \item{disaggregate}{components of the indicator that take direction from the MER}
#'   \item{otherdisaggregate}{additional components of the indicator, namely days for SDI}
#' }

"ind_map_bdi"
