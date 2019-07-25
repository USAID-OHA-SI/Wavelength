#' Gernate HTS_TST Total Numerator from POS and NEG
#'
#' @param df HFR processed dataframe to create a HTS total numerator
#'
#' @export

gen_hts_num <- function(df){

  #checks
  if(any(!var_exists(df, c("indicator", "val"))))
    stop("Missing `indicator` or `val`")

  if(!all(c("HTS_TST_POS", "HTS_TST_NEG") %in% unique(df$indicator)))
    stop("Missing HTS_TST_POS or HTS_TST_NEG")

  if(var_exists(df, "HTS_TST"))
    stop("HTS_TST already exists")

  #create HTS_TST (numerator)
  group_vars <- setdiff(names(df), "val")
  df_hts <- df %>%
    dplyr::filter(indicator %in% c("HTS_TST_POS", "HTS_TST_NEG")) %>%
    dplyr::mutate(indicator = "HTS_TST",
                  resultstatus = NA) %>%
    dplyr::group_by_at(group_vars) %>%
    dplyr::summarise_at(dplyr::vars(val), sum, na.rm = TRUE) %>%
    dplyr::ungroup()

  #remove HTS_NEG and bind on HTS_TST (numerator)
  df <- df %>%
    dplyr::filter(indicator != "HTS_TST_NEG") %>%
    dplyr::bind_rows(df_hts)

  return(df)
}
