#' Extract Headers
#'
#' @param filepath full filepath to the country file
#' @param sheetname tab name
#' @param skip_lines number of lines to skip
#'
#' @export
#'
#' @examples
#' #' \dontrun{
#' #get new header row
#'   path <- "~/WeeklyData/Cameroon_data.xlsx"
#'   headers <- extact_headers(path, "TX_NEW", 1) }

extact_headers <- function(filepath, sheetname, skip_lines){
  readxl::read_excel(filepath, sheet = sheetname,
                     skip = skip_lines, .name_repair = "minimal") %>%
    names()
}
