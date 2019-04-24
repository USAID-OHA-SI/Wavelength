#' Import and Structure DRC
#'
#' @description This function will be used to take the monthly DRC
#' dataset and structure in a uniform way to be tidy and align with other
#' OU datasets. Only selected indicators will be imported and transformed
#' based on OHA's needs. The data is sourced from DRC's partner's monthly
#' reporting template files.
#'
#' @param filepath filepath to the output file
#' @param folderpath_output if a txt output is desired, provide the full path to the folder
#'
#' @export
#'
#' @examples
#' \dontrun{
#' #structure output for DRC
#'   path <- "~/WeeklyData/Raw/DRC_montly_partner_x.xlsx"
#'   structure_cod(path)
#' #structure output for DRC & export to txt file
#'   structure_cod(path, "~/WeeklyData/Output") }

structure_cod <- function(filepath, folderpath_output = NULL){

  #import
    sheets <- c("HTS_TST", "HTS_TST Index", "TX_CURR", "TX_NEW") %>% purrr::set_names()

    df <- purrr::map_dfr(.x = sheets,
                         .f = ~ readxl::read_excel(filepath, sheet = .x, col_types = "text"),
                         .id = "indicator")
  #check structure
    #TODO add assert check to make sure structure stays the same

  #clean var names and values
    df <- df %>%
      dplyr::select(-dplyr::contains("Total"), -dplyr::contains("dif"), -pos, -dplyr::start_with("test")) %>%
      dplyr::rename_all(~stringr::str_replace_all(.,"\\r|\\n", " ") %>% stringr::str_squish()) %>%
      dplyr::mutate_all(~ dplyr::na_if(., 0))

  #fix different site names between sheets
    df <- df %>%
      dplyr::mutate(Site = ifelse(is.na(Site), `Site/Age Group`, Site)) %>%
      dplyr::select(-`Site/Age Group`)

  #reshape long & convert to numeric
    meta <- dplyr::select(df, indicator:Site) %>% names()
    df <- df %>%
      reshape_long(meta) %>%
      dplyr::rename(disagg = ind)

  #tidy age/sex/resulstatus
    df <- df %>%
      dplyr::mutate(otherdisaggregate = stringr::str_extract(disagg, "Community"),
                    disagg = stringr::str_replace(disagg, "Community Testing", "Unknown Age"),
                    disagg = stringr::str_replace_all(disagg, "(1|9) (P|N)", "\\1 Unknown Sex \\2"),
                    disagg = stringr::str_replace_all(disagg, "(1|9)$", "\\1 Unknown Sex"),
                    disagg = stringr::str_replace(disagg, "Unknown (Age|Sex)", "Unknown_\\1")) %>%
      tidyr::separate(disagg, c("agesemifine", "sex", "resulstatus"), sep = " ", fill = "right") %>%
      dplyr::mutate_at(dplyr::vars(agesemifine, sex), ~ stringr::str_replace(., "_", " ")) %>%
      dplyr::mutate(agesemifine = dplyr::case_when(agesemifine == "<1" ~ "<01",
                                                   agesemifine == "1-9" ~ "01-09",
                                                   agesemifine == ">=50" ~ "50+",
                                                   TRUE ~ agesemifine))

  #rename HTS_INDEX
    df <- dplyr::mutate(df, indicator = dplyr::case_when(indicator == "HTS_TST Index" & otherdisaggregate == "Community" ~ "HTS_INDEX_COM",
                                                  indicator == "HTS_TST Index"  ~ "HTS_INDEX_FAC",
                                                  TRUE ~ indicator))

  #add on additional age bands
    df <- apply_agebands(df)

  #add disaggregate
    df <- df %>%
      tibble::add_column(disaggregate = "Age/Sex", .after = "Site") %>%
      dplyr::mutate(disaggregate = ifelse(stringr::str_detect(indicator, "^HTS"), "Age/Sex/Result", "Age/Sex"))

  #clean variable names
    df <- df %>%
      dplyr::rename(fy = `Fiscal Year`,
                    month = Month,
                    mechanismid = `IM ID`,
                    snu1 = `Province`,
                    psnu = `Health Zone`,
                    facility = Site)

  #add operatingunit
    df <- add_ou(df, "Democratic Republic of the Congo")

    #add reporting frequency
    df <- tibble::add_column(df, reporting_freq = "Monthly",
                             after = "month")

  #TODO arrange variable order

  #export
    export_hfd(df, folderpath_output)

    return(df)
}
