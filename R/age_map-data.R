#' MER Age Mapping Table
#'
#' Table that cross references the different MER reporting
#' age bands, fine, semi-fine, and coarse. Some countries may
#' report at higher levels for weekly/monthly data collection
#' or may have started using different age bands in FY18 when
#' collection began
#'
#' @docType data
#'
#' @usage data(age_map)
#'
#' @format A data frame including all relevant MER agebands
#' \describe{
#'   \item{agecoarse}{highest level of age bands, <15/15+}
#'   \item{agesemifine}{more granular, used prior to FY19, include 40-49}
#'   \item{agefine}{most granular, provided mostly 5 year age bands}
#' }

"age_map"
