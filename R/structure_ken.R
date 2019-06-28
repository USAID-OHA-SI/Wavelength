#' Import and Structure Kenya
#'
#' @description This function will be used to take the Kenya HFR
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
#' #structure output for Kenya
#'   path <- "~/WeeklyData/Raw/KEN_Weekly.xlsx"
#'   structure_ken(path)
#' #structure output for Kenya & export to txt file
#'   structure_ken(path, "~/WeeklyData/Output") }

structure_ken <- function(filepath, folderpath_output = NULL){

  #import
    r_skip <- ifelse(stringr::str_detect(filepath, "COGRI"), 1, 0)
    df <- readxl::read_excel(filepath, skip = r_skip, col_types = "text")


  #reshape long for Afya Ziwani
    if(stringr::str_detect(filepath, "Afya Ziwani")){
      meta <- meta <- dplyr::select(df, County:`Week Number`) %>% names()
      df <- df %>%
        reshape_long(meta) %>%
        dplyr::mutate(ind = stringr::str_replace_all(ind, " (Unknown|Below 1|Above 50|[:digit:]|<|Male|Female|M$|F$)", "\\.\\1")) %>%
        tidyr::separate(ind, c("ind", "agesemifine", "sex"), sep = "\\.", fill = "right") %>%
        dplyr::mutate(sex = dplyr::recode(sex, M = "Male", F = "Female"),
                      agesemifine = dplyr::case_when(agesemifine == "Below 1"   ~ "<01",
                                                     agesemifine == "1-9"       ~ "01-09",
                                                     agesemifine == "Above 50"  ~ "50+",
                                                     TRUE                       ~ agesemifine))
    }

  #reshape long for AfyaKamilisha
    if(stringr::str_detect(filepath, "AfyaKamilisha")){
      meta <- dplyr::select(df, -dplyr::contains("15")) %>% names()
      df <- df %>%
        reshape_long(meta) %>%
        tidyr::separate(ind, c("indicator", "agecoarse", "sex"), sep = " ") %>%
        dplyr::mutate(sex = dplyr::recode(sex, m = "Male", f = "Female"))
    }

  #reshape long for COGRI
    if(stringr::str_detect(filepath, "COGRI")){
      df <- df %>%
        tidyr::gather(agesex, val, dplyr::contains("15")) %>%
        tidyr::separate(agesex, c("agecoarse", "sex"), sep = " yrs ") %>%
        dplyr::mutate(agecoarse = ifelse(agecoarse == ">15", "15+", agecoarse) %>%
                        stringr::str_remove(" "),
                      sex = dplyr::recode(sex, M = "Male", F = "Female"))
    }

  #clean up column names
    df <- clean_vars_ken(df)

  #clean up mechanism ID, FY and value
    char_vars <- dplyr::case_when(stringr::str_detect(filepath, "COGRI")       ~ c("mechanismid", "fy"),
                                  stringr::str_detect(filepath, "Afya Ziwani") ~ "fy",
                                  TRUE                                         ~ "mechanismid")
    num_vars <- ifelse(stringr::str_detect(filepath, "COGRI|Afya Ziwani"), "val", c("val", "fy"))
    df <- df %>%
      dplyr::mutate_at(dplyr::vars(char_vars), ~ as.integer(.) %>% as.character()) %>%
      dplyr::mutate_at(dplyr::vars(num_vars), as.numeric)

  #add OU, coarse age & map indicator info and drop extra indicators Afya Ziwani
    if(stringr::str_detect(filepath, "Afya Ziwani")){

      #add coarse age
      df <- age_map %>%
        dplyr::distinct(agecoarse, agesemifine) %>%
        dplyr::left_join(df, ., by = "agesemifine") %>%
        dplyr::mutate(agecoarse = ifelse(agesemifine == "Unknown", "Unknown", agecoarse))
      #map indicator info and drop extra indicators
      df <- df %>%
        dplyr::left_join(ind_map_ken, by = "ind") %>%
        dplyr::filter(!is.na(indicator)) %>%
        dplyr::select(-ind)
    }

  #remove "yrs" from age and adjust 15+
    df <- df %>%
      dplyr::mutate(agecoarse = stringr::str_remove_all(agecoarse, "(y|Y)rs") %>%
                          stringr::str_trim(),
                    agecoarse = ifelse(agecoarse == ">15", "15+", agecoarse))

  #fix excel date
    df <- fix_dates_ken(df, filepath)

  #add OU if it doesn't exist
    if(!var_exists(df, "operatingunit"))
       df <- add_ou(df, "Kenya")

  #fix case issue
    case_vars <- ifelse(var_exists(df, "reporting_freq"), c("operatingunit", "reporting_freq"), "operatingunit")
    df <- dplyr::mutate_at(df, dplyr::vars(case_vars), stringr::str_to_sentence)

  #add disaggregate
    if(!var_exists(df, "disaggregate"))
      df <- dplyr::mutate(df, disaggregate = "Age/Sex")

  #standardize variable order
    df <- order_vars(df)

  #export or return
    export_hfr(df, folderpath_output)

  invisible(df)
}


#' Rename Variables to Unify Kenya Parnter HFR Submissions
#'
#' @param df Kenya HFR dataframe

clean_vars_ken <- function(df){

  df <- df %>%
    dplyr::rename_all(stringr::str_replace_all, "( |-)", "_") %>%
    dplyr::rename_all(tolower)

  if(var_exists(df, "orgunitlevel1"))
    df <- dplyr::rename(df, operatingunit = orgunitlevel1)

  if(var_exists(df, "county"))
    df <- dplyr::rename(df, snu1 = county)

  if(var_exists(df, "ward"))
    df <- dplyr::rename(df, psnu = ward)

  if(var_exists(df, "value"))
    df <- dplyr::rename(df, val = value)

  if(var_exists(df, "values"))
    df <- dplyr::rename(df, val = values)

  if(var_exists(df, "age_group"))
    df <- dplyr::rename(df, agecoarse = age_group)

  if(var_exists(df, "age"))
    df <- dplyr::rename(df, agecoarse = age)

  if(var_exists(df, "gender"))
    df <- dplyr::rename(df, sex = gender)

  if(var_exists(df, "fy19"))
    df <- dplyr::rename(df, fy = fy19)

  if(var_exists(df, "facilityuid") && !var_exists(df, "orgunituid"))
    df <- dplyr::rename(df, orgunituid = facilityuid)

  if(var_exists(df, "financial_year"))
    df <- dplyr::rename(df, fy = financial_year)

  df <- dplyr::select(df, -dplyr::matches("mfl|sub_county|subcounty"))

  return(df)
}

#' Check if variable exist
#'
#' @param df data frame to check against
#' @param var quoted variable of interest

var_exists <- function(df, var) {

  var %in% names(df)

}

#' Conform dates from different partners
#'
#' @param df Kenya HFR data frame
#' @param filepath Kenya HFR file path

fix_dates_ken <- function(df, filepath){
  if(stringr::str_detect(filepath, "Afya Pwani|AMPATHPlus|HCM")){

    df <- dplyr::mutate(df, date = lubridate::as_date(as.integer(date), origin = "1899-12-30"))

  } else if (stringr::str_detect(filepath, "Afya Nyota|COGRI")){

    df <- dplyr::mutate(df, date = lubridate::as_date(date))

  } else if(stringr::str_detect(filepath, "AfyaKamilisha")){

    df <- df %>%
      dplyr::mutate(date = paste0(stringr::word(date, -1), " ",
                                  stringr::str_extract(date, "^[:digit:]{1,2}"),
                                  ", 2019") %>%
                      lubridate::mdy())

  } else if(stringr::str_detect(filepath, "Afya Ziwani")){

    df <- df %>%
      dplyr::mutate(date = lubridate::ymd("2018-10-01") +
                      lubridate::weeks(as.numeric(week_number) - 1)) %>%
      dplyr::select(-week_number)

  } else {

    stop("Unable to match partner from file name")

  }

  return(df)
}
