#' Validation Checks
#'
#' @param df HFR data framed created by `hfr_process_template()`
#'
#' @export

validate_output <- function(df){

    check_output_cols(df)
    check_dates(df)
    check_orgunituids(df)
    check_mechs(df)
    check_inds(df)
    check_disaggs(df)

    }


#' Validate columns for export
#'
#' @param df HFR data framed created by `hfr_process_template()`

check_output_cols <-function(df){

  #check headers
    req_cols <- c("date", "fy", "hfr_pd", "orgunit",
                  "orgunituid", "mech_code", "partner",
                  "operatingunit", "psnu", "indicator",
                  "agecoarse", "sex", "otherdisaggregate", "val")

    submitted <- names(df)

  #missing columns
   missing <- flag_missing(req_cols, submitted)

  #extra columns
  extra <- flag_extra(req_cols, submitted)

  #print validation
    cat("\nAre there any missing columns for export?", missing,
        "\nAre there any extra columns for export?", extra)
}


#' Validate dates
#'
#' @param df HFR data framed created by `hfr_process_template()`

check_dates <-function(df){

  #missing dates?
    missing_dates <- count_missing(df, date)

  #date range
    pds <- length(unique(df$hfr_pd))

    date_range <- df %>%
      dplyr::distinct(date, fy, hfr_pd) %>%
      dplyr::mutate(hfr_pd = hfr_pd/100,
                    pd = paste0("[", fy + hfr_pd, "]")) %>%
      tidyr::unite(date_pd, c("date", "pd"), sep = " ") %>%
      dplyr::pull(date_pd) %>%
      paste(collapse = ", ")
    date_range <- ifelse(pds > 1, crayon::red(date_range), crayon::green(date_range))
    pds <- ifelse(pds > 1, crayon::red("Yes"), crayon::green("No"))

  #print validation
  cat("\nAre there any missing dates?", missing_dates,
      "\nDoes the submission cover multiple period?", pds,
      "\nWhat dates does the submission cover?", date_range)
}



#' Validate orgunituids for export
#'
#' @param df HFR data framed created by `hfr_process_template()`

check_orgunituids <-function(df){

  #missing orgunituid?
    missing_orgunituid <- count_missing(df, orgunituid)

  #print validation
    cat("\nAre there any missing orgunituids?", missing_orgunituid)
}


#' Validate mechanisms for export
#'
#' @param df HFR data framed created by `hfr_process_template()`

check_mechs <-function(df){

  #missing mechanisms?
    missing_mechs<- count_missing(df, mech_code)

  #mechanisms
    mech_list <- unique(df$mech_code) %>% sort() %>% paste(collapse = ", ") %>% crayon::blue()

  #print validation
  cat("\nAre there any missing mech_codes?", missing_mechs,
      "\nWhat mechanism are included?", mech_list)
}


#' Validate indicators for export
#'
#' @param df HFR data framed created by `hfr_process_template()`

check_inds <-function(df){

    missing_ind <- count_missing(df, indicator)

  #indicators
    req <- c("HTS_TST", "HTS_TST_POS", "PrEP_NEW",
                 "TX_CURR", "TX_MMD", "TX_NEW", "VMMC_CIRC")
    sumbitted <- unique(df$indicator)

    missing <- flag_missing(req, sumbitted)
    extra <- flag_extra(req, sumbitted)


  #print validation
  cat("\nAre there any unspecified indicators?", missing_ind,
      "\nAre there any missing indicators?", missing,
      "\nAre there any extra indicators?", extra)
}


#' Validate disaggs for export
#'
#' @param df HFR data framed created by `hfr_process_template()`

check_disaggs <-function(df){

  #age/sex
    req <- c("Female <15", "Female 15+", "Male <15", "Male 15+")
    submitted <- df %>%
      dplyr::distinct(agecoarse, sex) %>%
      tidyr::unite(agesex, c("sex", "agecoarse"), sep = " ") %>%
      dplyr::pull(agesex)

    missing <- flag_missing(req, submitted)
    extra <- flag_extra(req, submitted)

  #MMD months
    req_otherdisagg <- c("<3 months", "3-5 months", "6 months or more")
    sumbitted_otherdisagg <- unique(df$otherdisaggregate) %>% setdiff(NA)

  #extra otherdisaggs
    extra_otherdisagg <- flag_extra(req_otherdisagg, sumbitted_otherdisagg)

  #print validation
  cat( "\nAre there any missing age/sex disaggs?", missing,
       "\nAre there any extra age/sex disaggs?", extra,
       "\nAre there any extra other disaggs?", extra_otherdisagg, "\n")
}

