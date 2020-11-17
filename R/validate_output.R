#' Validation Checks
#'
#' @param df HFR data framed created by `hfr_process_template()`
#' @param content check full dataset
#' @param datim_path path to look up files
#'
#' @export

validate_output <- function(df, output_path, content=FALSE, datim_path=NULL){

    check_output_cols(df)
    check_dates(df)
    check_orgunituids(df)
    check_mechs(df)
    check_inds(df)
    check_disaggs(df)

    #optional check
    if (content & !is.null(datim_path)) {
        df <- check_content(df, output_path, datim_path)
    }

    return(df)
}


#' Validate columns for export
#'
#' @param df HFR data framed created by `hfr_process_template()`

check_output_cols <- function(df){

  #check headers
    req_cols <- c("date", "fy", "hfr_pd", "hfr_freq","orgunit",
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

check_disaggs <- function(df){

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


#' Validate output content
#'
#' @param df HFR DataFrame
#' @param datim_path path of datim lookup files
#' @export
#' @return df updated HFR dataframe
#'
check_content <- function(df, output_path, datim_path) {

  cat("\nLoading lookup tables ...\n")

  # Load lookup tables: load only once
  if ( !exists("orgs") | !exists("ims") ) {
    load_lookups(datim_path)
  }

  cat("\nChecking operatingunits values ...")

  # Check and update operatingunits
    err_ou <- df %>%
      is_ou_valid(df_orgs = orgs) %>%
      dplyr::filter(!valid_ou) %>%
      dplyr::select(-valid_ou) %>%
      dplyr::distinct(operatingunit) %>%
      dplyr::pull()

    if ( length(err_ou) > 0 ) {

      cat("\nAre there any invalid operatingunits?", ifelse(length(err_ou) > 0, paint_red("Yes"), paint_green("No")),
          "\nList of invalid operatingunits: ", paint_red(paste(err_ou, collapse = ", ")))

      cat("\nUpdating operatingunit from mech codes ...")

      # transform & extract unique mech codes
      ims_ou <- ims %>%
        dplyr::mutate(mech_code = as.character(mech_code)) %>%
        dplyr::select(mech_code , ou = operatingunit) %>%
        dplyr::distinct(mech_code, ou)

      df <- df %>%
        is_ou_valid(df_orgs = orgs) %>%
        dplyr::mutate(mech_code = as.character(mech_code)) %>%
        dplyr::left_join(ims_ou, by=c("mech_code" = "mech_code")) %>%
        dplyr::mutate(operatingunit = ifelse(valid_ou == FALSE, ou, operatingunit)) %>%
        dplyr::select(-c(valid_ou, ou))

      #Check again after update
      err_ou <- df %>%
        is_ou_valid(df_orgs = orgs) %>%
        dplyr::filter(!valid_ou) %>%
        dplyr::select(-valid_ou) %>%
        dplyr::distinct(operatingunit) %>%
        dplyr::pull()

      cat("\nAre there still any invalid operatingunit?", ifelse(length(err_ou) > 0, paint_red("Yes"), paint_green("No")),
          "\nList of invalid operatingunit: ", ifelse(length(err_ou) > 0, paint_red(paste(err_ou, collapse = ", ")), paint_green("None")))
    }

  # Check the rest of the data
    cat("\nChecking the entire dataset ... XXXX")

    df <- df %>%
      is_ou_valid(df_orgs = orgs) %>%
      is_mech_valid(df_mechs = ims) %>%
      is_mech4ou(df_mechs = ims) %>%
      is_orgunituid_valid(df_orgs = orgs) %>%
      is_orgunituid4ou(df_orgs = orgs) %>%
      dplyr::mutate(
        valid_age = ifelse(is.na(agecoarse) | agecoarse %in% valid_age, TRUE, FALSE),
        valid_sex = ifelse(is.na(sex) | sex %in% valid_sex, TRUE, FALSE),
        valid_value = ifelse(is.na(val) | is.integer(val) | val >= 0, TRUE, FALSE)
      )

    # Sum up invalid columns
    grp <- df %>%
      dplyr::select(-c(date:val)) %>%
      names()

    df <- df %>%
      rowwise() %>%
      mutate(errors = sum(all_of(grp) == FALSE)) %>%
      ungroup()

    # Errors count
    n_errors <- df %>%
      dplyr::filter(errors > 0) %>%
      dplyr::distinct(mech_code) %>%
      dplyr::pull() %>%
      length()

    if ( n_errors > 0 ) {
      msg_errors <- paint_red('Yes')
    } else {
      msg_errors <- paint_green('No')
    }

    cat("\nAre there any mechanism with invalid data?", msg_errors)

    if (n_errors > 0) {

      cat("\nList of mechanisms with errros: ", paint_red(paste(errors, collapse = ", ")))

      df %>%
        dplyr::group_by(mech_code) %>%
        dplyr::mutate(row_id = dplyr::row_number()) %>%
        dplyr::ungroup() %>%
        dplyr::filter(errors > 0) %>%
        readr::write_csv(., paste0(output_path,
                                   "/HFR_ERRORS_", curr_fy, ".", stringr::str_pad(dplyr::first(df$hfr_pd), 2, pad="0"), "_",
                                   paste(errors, collapse = "_"), "_",
                                   format(Sys.Date(),"%Y%m%d"), ".csv"), na="")

      cat("\nThe errors file is located here: ", paint_blue(datim_path))
    }

    df <- df %>%
      dplyr::select(date:val, errors) %>%
      dplyr::mutate(errors = ifelse(errors > 0, TRUE, FALSE))

    return(df)
}
