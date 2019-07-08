#' Malawi Indicator Mapping Table
#'
#' Table that cross references between partner data
#' collected/reported to the mission weekly/monthly
#' and the PEPFAR MER indicators from worksheets.
#'
#' @docType data
#'
#' @usage data(ind_map_mwi)
#'
#' @format A data frame including all relevant indicators
#' \describe{
#'   \item{ind}{indicator reported by IM}
#'   \item{indicator}{standardized indicator to work across partners and OUs}
#'   \item{disaggregate}{components of the indicator that take direction from the MSD}
#'   \item{agecoarse}{age, <15/15+}
#'   \item{agefine, <01-50+}
#'   \item{sex}{sex, Male or Female}
#'   \item{resultstatus}{HIV status, Positive/Negative}
#' }

"ind_map_mwi"
