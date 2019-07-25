#' Import and Structure Malawi
#'
#' @description This function will be used to take the Malawi HFR
#' dataset and structure in a uniform way to be tidy and align with other
#' OU datasets. Only selected indicators will be imported and transformed
#' based on OHA's needs.
#'
#' @param filepath filepath to the output file
#' @param folderpath_output if a txt output is desired, provide the full path to the folder
#'
#' @export
#'
#' @examples
#' \dontrun{
#' #structure output for Malawi
#'   path <- "~/WeeklyData/Raw/MWI_Weekly.xlsx"
#'   structure_eth(path)
#' #structure output for Malawi & export to txt file
#'   structure_eth(path, "~/WeeklyData/Output") }

structure_mwi <- function(filepath, folderpath_output = NULL){

  if(stringr::str_detect(filepath, "Facility")){

    #import
    df <- filepath %>%
      readxl::excel_sheets() %>%
      purrr::map_dfr(~ readxl::read_excel(filepath, sheet = .x, col_types = "text"))

    #fix mechanism id
    df <- df %>%
      dplyr::mutate(mechanismid = ifelse(!is.na(MechanismNumber), MechanismNumber, MechanismID)) %>%
      dplyr::select(-MechanismNumber, -MechanismID)

    #reshape
    meta <- c("mechanismid", "Partner","FundingAgency", "OperatingUnit", "DISTRICT",
              "SITE", "OrganisationUnitId", "BeginWeekDateddmmyyyy")
    df <- df %>%
      dplyr::select(-c(FacilityID, MechanismName, HTS_TST, HTS_TST_POS, HTS_TST_NEG, TX_NEW)) %>%
      reshape_long(meta) %>%
      dplyr::rename_all(tolower) %>%
      dplyr::rename(psnu = district,
                    facility = site,
                    orgunituid = organisationunitid,
                    date = beginweekdateddmmyyyy)

    #fix indicator names and disaggs
    df <- df %>%
      dplyr::mutate(ind = stringr::str_replace(ind, "UNKAGE(FEMALE|MALE)$", "tx\\1Unknown Age"),
                    ind = stringr::str_replace(ind, "UNKAGE(FEMALE|MALE)(Positive|Negative)", "HTS_\\2\\1Unknown Age"),
                    ind = stringr::str_remove(ind, "(i|a)tive"),
                    ind = stringr::str_replace(ind, "(FEMALE|female|MALE|male)", "\\.\\1\\."),
                    ind = stringr::str_replace(ind, "HTS_", "HTS_TST_"),
                    ind = stringr::str_replace(ind, "tx", "TX_NEW")) %>%
      tidyr::separate(ind, c("indicator", "sex", "agecoarse"), sep = "\\.") %>%
      dplyr::mutate(indicator = toupper(indicator),
                    sex = stringr::str_to_sentence(sex),
                    agecoarse = stringr::str_replace(agecoarse, "(p|P)lus", "+"),
                    agecoarse = stringr::str_replace(agecoarse, "U15", "<15"),
                    disaggregate = "Age/Sex")

  } else {

    #import disperate sheets
    df <- filepath %>%
      readxl::excel_sheets() %>%
      purrr::set_names() %>%
      purrr::map_dfr(~ import_mwi_comm(filepath, .x))

    #adjust indicators and disaggs
    df <- df %>%
      dplyr::mutate(ind = stringr::str_remove(ind, "- |-"),
                    ind = stringr::str_replace(ind, "\\+", "99"),
                    ind = ifelse(stringr::str_detect(ind, "TX_NEW"),
                                 paste("TX_NEW",
                                       stringr::str_extract(ind, "(FEMALE|MALE)"),
                                       stringr::str_extract(ind, "[:digit:]{4}"), sep = "."),
                                 ind),
                    ind = stringr::str_replace(ind, "U15", "\\..<15"),
                    ind = stringr::str_replace(ind, "15Plus", "\\..15+"),
                    ind = stringr::str_replace(ind, "5099|O50", "50+"),
                    ind = stringr::str_replace(ind, "(POS|neg)", "TST_\\1\\."),
                    ind = stringr::str_replace(ind, "(15P|U15)", "\\.\\1"),
                    ind = stringr::str_replace(ind, " (FEMALE|MALE)", "\\.\\1"),
                    ind = toupper(ind)) %>%
      tidyr::separate(ind, c("indicator", "sex", "agefine", "agecoarse"), sep = "\\.", fill = "right") %>%
      dplyr::mutate(agecoarse = ifelse(is.na(agecoarse), "15+", agecoarse),
                    sex = stringr::str_to_sentence(sex)) %>%
      dplyr::select(-agefine)

    #aggregate
    group_vars <- setdiff(names(df), "val")
    df <- df %>%
      dplyr::group_by_at(dplyr::vars(group_vars)) %>%
      dplyr::summarise(val = sum(val, na.rm = TRUE)) %>%
      dplyr::ungroup()
  }


  #creat HTS_TST
    df <- gen_hts_num(df)

  #fix date
    df <- df %>%
      dplyr::mutate(date = ifelse(date == "43617", "43619", date),  #fix wrong reporting pd 6/1 -> 6/3
                    date = as.Date(as.numeric(date), origin = "1899-12-30")) %>%
      assign_pds()

  #add disaggregete
    df <- dplyr::mutate(df, disaggregate = "Age/Sex")

  #add frequency
    df <- dplyr::mutate(df, reporting_freq = "Weekly")

  #standardize variable order
    df <- order_vars(df)

  #export
    export_hfr(df, folderpath_output)

    invisible(df)
  }

#' Import disperate community sheets
#'
#' @param filepath Malawi HFR Community excel filepath
#' @param sht sheet name
#'
#' @export

import_mwi_comm <- function(filepath, sht){

  skip_rows <- ifelse(sht %in% c("HTS_TST", "TX_NEW"), 0, 1)

  df <- readxl::read_excel(filepath, sheet = sht, skip = skip_rows, col_types = "text")

  df <- clean_vars_mwi(df)

  if(sht == "VMMC")
    df <-dplyr::rename_all(df, ~ stringr::str_replace(., "PREP_NEW", "VMMC_CIRC"))

  #remove vars
  keep_vars <- setdiff(names(df),
                       c("HTS_TST", "HTS_TST_POS", "HTS_TST_NEG", "FACILITY_ID",
                         "VMMC_CIRC", "PREP.TOTAL","TX_CURR.TOTAL", "TX_NEW",
                         "Mechanism Name", "MechanismName"))
  df <- dplyr::select_at(df, dplyr::vars(keep_vars))


  meta <- df %>%
    dplyr::select(dplyr::matches("Partner|FundingAgency|OperatingUnit|psnu|facility|orgunituid|MechanismID|date")) %>%
    names()

  df <- reshape_long(df, meta)

  df <- dplyr::rename_all(df, tolower)

  if(sht == "TX_NEW")
    df <- dplyr::mutate(df, ind = paste0("TX_NEW.", ind))

  return(df)
}


#' Clean Malawi variables
#'
#' @param df Malawi Community HFR dataset
#'
#' @export

clean_vars_mwi <- function(df){

  if(var_exists(df, "MECHANISM_NAME"))
    df <- dplyr::rename(df, partner = MECHANISM_NAME)

  if(var_exists(df, "Mechanism Name") && !var_exists(df, "Partner"))
    df <- dplyr::rename(df, partner = `Mechanism Name`)

  if(var_exists(df, "District"))
    df <- dplyr::rename(df, DISTRICT = District)

  if(var_exists(df, "MECHANISM_ID"))
    df <- dplyr::rename(df, MechanismID = MECHANISM_ID)

  if(var_exists(df, "Mechanism ID"))
    df <- dplyr::rename(df, MechanismID = `Mechanism ID`)

  if(var_exists(df, "PSNUId"))
    df <- dplyr::rename(df, orgunituid = PSNUId)

  if(var_exists(df, "FACILITY_UID"))
    df <- dplyr::rename(df, orgunituid = FACILITY_UID)

  if(var_exists(df, "OrganisationUnitId"))
    df <- dplyr::rename(df, orgunituid = OrganisationUnitId)

  if(var_exists(df, "Begin Week Date (dd/mm/yyyy)"))
    df <- dplyr::rename(df, date = `Begin Week Date (dd/mm/yyyy)`)

  if(var_exists(df, "DATE_WEEK_START"))
    df <- dplyr::rename(df, date = DATE_WEEK_START)

  if(var_exists(df, "SITE"))
    df <- dplyr::rename(df, facility = SITE)

  if(!var_exists(df, "OperatingUnit"))
    df <- dplyr::mutate(df, OperatingUnit = "Malawi")

  if(!var_exists(df, "FundingAgency"))
    df <- dplyr::mutate(df, FundingAgency = "USAID")

  df <- df %>%
    dplyr::rename(psnu = DISTRICT)

  return(df)
}
