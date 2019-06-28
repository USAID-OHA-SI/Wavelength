#' Kenya Indicator Mapping Table
#'
#' Table that cross references between partner data
#' collected/reported to the mission weekly/monthly
#' and the PEPFAR MER indicators from worksheets.
#' Needed for only one mechanism, Afya Ziwani.
#'
#' @docType data
#'
#' @usage data(ind_map_ken)
#'
#' @format A data frame including all relevant indicators
#' \describe{
#'   \item{ind}{indicator reported by IM}
#'   \item{indicator}{standardized indicator to work across partners and OUs}
#'   \item{disaggregate}{components of the indicator that take direction from the MSD}
#'   \item{modality}{HIV entry point}
#'   \item{resultstatus}{HIV status, Positive or NA}
#' }

"ind_map_ken"
