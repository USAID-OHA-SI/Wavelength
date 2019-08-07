#' Import and Structure Mozambique
#'
#' @description This function will be used to take the Mozambique HFR
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
#' #structure output for Mozambique
#'   path <- "~/WeeklyData/Raw/MOZ_Weekly.xlsx"
#'   structure_moz(path)
#' #structure output for Mozambique & export to txt file
#'   structure_moz(path, "~/WeeklyData/Output") }

structure_moz <- function(filepath, folderpath_output = NULL){

  #import sheets
    df <- filepath %>%
      readxl::excel_sheets() %>%
      purrr::map_dfr( ~ readxl::read_excel(filepath, sheet = .x, col_types = "text"))

  if(var_exists(df, "Partner (COP19)")){
    #clean up
      meta <- c("Date", "Datim ID", "Partner (COP19)", "Mechanism ID",
                "Agency (COP19)", "Province", "District", "Site Name")
      df <- df %>%
        dplyr::select(-c(`Sisma ID`, AJUDA, DataSource), -dplyr::contains("NET_NEW")) %>%
        reshape_long(meta)

    #rename columns
      df <- df %>%
        dplyr::rename_all(~ stringr::str_remove_all(. ," ") %>% tolower) %>%
        dplyr::rename(orgunituid = datimid,
                      partner = `partner(cop19)`,
                      fundingagency = `agency(cop19)`,
                      snu1 = province,
                      psnu = district,
                      facility = sitename)

    #adjust variables and disaggregates
      df <- df %>%
        tidyr::separate(ind, c("indicator", "agecoarse"), sep = " ", fill = "right") %>%
        dplyr::mutate(indicator = dplyr::case_when(indicator == "3MD" ~ "TX_MMD",
                                                   indicator == "VMMC" ~ "VMMC_CIRC",
                                                   TRUE ~ indicator),
                      otherdisaggregate = dplyr::case_when(indicator == "TX_MMD" ~ "3 months"),
                      disaggregate = dplyr::case_when(indicator == "TX_MMD" ~ "Period",
                                                      !is.na(agecoarse) ~ "Age/Sex",
                                                      TRUE ~ "Total Numerator"))
  } else {

    #clean up
      df <- df %>%
        dplyr::mutate(date = ifelse(!is.na(date), date, week),
                      orgunituid = ifelse(!is.na(Datim_code), Datim_code, datim_id),
                      facility = ifelse(!is.na(facility), facility, site),
                      indicator = ifelse(!is.na(indicator), indicator, area),
                      val = ifelse(!is.na(val), val, value),
                      partner = ifelse(!is.na(partner), partner, source)
                      ) %>%
        dplyr::select(-c(sisma_id, source, sector, Datim_code, datim_id, site, value, area, month, week))

    #rename columns
      df <- df %>%
        dplyr::rename(snu1 = province,
                      psnu = district,
                      resultstatus = test_result)

    #adjust HTS indicator names
      df <- df %>%
        dplyr::mutate(resultstatus = dplyr::na_if(resultstatus, "NA"),
                      indicator = ifelse(!is.na(resultstatus), paste0(indicator, "_", toupper(resultstatus)), indicator))

    #adjust age & result status
      df <- df %>%
        dplyr::mutate(agecoarse = dplyr::case_when(age == "Total Numerator" ~ as.character(NA),
                                                   age %in% c("0.1","1.9", "10.14", "0.14000000000000001") ~ "<15",
                                                   TRUE ~ "15+"),
                      resultstatus = dplyr::recode(resultstatus, "Neg" = "Negative", "Pos" = "Positive")) %>%
        dplyr::select(-age)

    #adjust value type
      df <- dplyr::mutate(df, val = as.double(val))

    #add HTS
      df <- gen_hts_num(df)

    #adjust indicators/disaggs
      df <- df %>%
        dplyr::filter(!indicator %in% c("TX_NET_NEW", "TX_DSD")) %>%
        dplyr::mutate(indicator = ifelse(indicator == "3MD", "TX_MMD", indicator),
                      otherdisaggregate = dplyr::case_when(indicator == "TX_MMD" ~ "3 months"),
                      disaggregate = dplyr::case_when(indicator == "TX_MMD" ~ "Period",
                                                      !is.na(agecoarse) ~ "Age/Sex",
                                                      TRUE ~ "Total Numerator"))
  }


  #fix date format
    df <- df %>%
      dplyr::mutate(date = ifelse(date == "43617", "43621", date),
                    date = lubridate::as_date(as.integer(date), origin = "1899-12-30") - 2) %>%
      dplyr::select(date, dplyr::everything()) %>%
      assign_pds()

  #add operating unit
    df <- add_ou(df, "Mozambique")

  #add reporting frequency
    df <- dplyr::mutate(df, reporting_freq = ifelse(indicator == "VMMC_CIRC", "Weekly", "Monthly"))

  #aggregate
    group_vars <- setdiff(names(df), "val")

    df <- df %>%
      dplyr::group_by_at(dplyr::vars(group_vars)) %>%
      dplyr::summarise_at(dplyr::vars(val), sum, na.rm = TRUE) %>%
      dplyr::ungroup()

  #standardize variable order
    df <- order_vars(df)

  #export
    export_hfr(df, folderpath_output)

  invisible(df)
}

