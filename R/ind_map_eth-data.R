#' Ethiopia Indicator Mapping Table
#'
#' Table that cross references between partner data
#' collected/reported to the mission weekly/monthly
#' and the PEPFAR MER indicators from worksheets.
#'
#' @docType data
#'
#' @usage data(ind_map_eth)
#'
#' @format A data frame including all relevant indicators
#' \describe{
#'   \item{ind}{indicator reported by IM}
#'   \item{indicator}{standardized indicator to work across partners and OUs}
#'   \item{disaggregate}{components of the indicator that take direction from the MSD}
#'   \item{sex}{sex, male or female}
#'   \item{agecoarse}{age, <15/15+}
#'   \item{resultstatus}{HIV status, Positive/Negative}
#'   \item{modality}{HIV entry point}
#'   \item{otherdisaggregate}{other components of the indicator not captured elsewhere}
#' }

"ind_map_eth"
