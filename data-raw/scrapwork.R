# Pull from DATIM ---------------------------------------------------------



ous <- c("Angola", "Botswana", "Burma", "Burundi", "Cameroon",
         "Cote d'Ivoire", "Democratic Republic of the Congo",
         "Dominican Republic", "Eswatini", "Ethiopia",
         "Ghana", "Haiti", "India", "Indonesia",
         "Kenya", "Lesotho", "Malawi", "Mozambique",
         "Namibia", "Nigeria", "Papua New Guinea", "Rwanda",
         "South Africa", "South Sudan", "Tanzania", "Uganda",
         "Ukraine", "Vietnam", "Zambia", "Zimbabwe")



get_em <- function(ou, folderpath_output = "C:/Users/achafetz/Downloads"){
  df <- extract_targets(ou_name = ou, username = myuser, password = mypwd(myuser))

  if(!is.null(df)){
    df <- periodize_targets(df, 2)

    iso <- dplyr::filter(iso_map, countryname == ou) %>%
      dplyr::pull(iso)

    filename <- paste0("HFR_DATIM_FY19Q2_", ou, "_", format(Sys.Date(),"%Y%m%d"), ".txt")

    readr::write_tsv(df, file.path(folderpath_output, filename), na = "")

    invisible(df)
  }

}

get_em("Thailand")

purrr::walk(ous, get_em)

files <- list.files("C:/Users/achafetz/Downloads", "HFR_DATIM", full.names = TRUE)


full_set <- purrr::map_dfr(files, readr::read_tsv, col_types = c(.default = "c"))
folderpath_output <- "C:/Users/achafetz/Downloads"
filename <- paste0("HFR_DATIM_FY19Q2_Global_", format(Sys.Date(),"%Y%m%d"), ".txt")
readr::write_tsv(full_set, file.path(folderpath_output, filename), na = "")


# Testing -----------------------------------------------------------------


combine_sources <- function(path_hfr, path_datim, hfr_pd, output_folder = "C:/Users/achafetz/Downloads/HFR_ready"){

  #import
  if(tools::file_ext(path_hfr) == "txt"){
    df_hfr <- readr::read_tsv(path_hfr, col_types = c(.default = "c")) %>%
      dplyr::mutate(date = lubridate::ymd(date))
  } else {
    df_hfr <- readr::read_csv(path_hfr, col_types = c(.default = "c")) %>%
      dplyr::mutate(date = lubridate::mdy(date))
  }

  if("site" %in% names(df_hfr)) df_hfr <- dplyr::rename(df_hfr, facility = site)

  df_mer <- readr::read_tsv(path_datim, col_types = c(.default = "c"))

  #rename org/partner info (HFR)
    # vars_dup <- c("operatingunit", "fundingagency", "snu1", "psnu", "facility") #"partner"
    # if("community" %in% names(df_hfr)) vars_dup <- c(vars_dup, "community")
    # vars_rename <- c("partner")

    df_hfr_adj <- df_hfr #%>%
      # dplyr::select(-vars_dup) %>%
      # dplyr::rename_at(dplyr::vars(vars_rename), ~ paste0(., "_hfr"))

  #duplicate targets for each week (DATIM)
    dates <- lubridate::as_date("2019-05-13") %>% seq(by = 7, length.out = 4) #unique(df_hfr_adj$date)

    df_mer_rpt <- purrr::map_dfr(.x = dates,
                   .f = ~dplyr::mutate(df_mer, date = .x))

  #join
    df_join <- dplyr::full_join(df_mer_rpt, df_hfr_adj) # , by = c("orgunituid", "fy", "mechanismid", "agecoarse", "sex", "indicator", "date"))

  #export
    ou <- df_join$operatingunit[1]
    if(stringr::str_detect(ou, "Reg")) ou <- df_join$snu1[1]
    iso <- dplyr::filter(iso_map, countryname == ou) %>%
      dplyr::pull(iso)

    filename <- paste0("HFR_JOINT_", iso, "_FY19P", hfr_pd, ".txt") %>% file.path(output_folder, .)
    readr::write_tsv(df_join, filename, na = "")

}



path_datim <- list.files("C:/Users/achafetz/Downloads/", "HFR_DATIM_FY19Q2_Thai", full.names = TRUE)
path_hfr <- list.files("C:/Users/achafetz/Downloads/", "HF(R|D).*THA_2", full.names = TRUE)

x <- combine_sources(path_hfr, path_datim, 9)


all <- list.files("C:/Users/achafetz/Downloads/HFR_ready", "JOINT", full.names = TRUE) %>%
  purrr::map_dfr(readr::read_tsv, col_types = c(.default = "c"))

#fix dates (Moz & Ukraine)
all <- all %>%
  dplyr::mutate(date = dplyr::case_when(date == "2019-06-20" ~ "2019-06-03",
                                        date == "2019-05-01" ~ "2019-04-29",
                                        TRUE ~ date))

#fix MMD indicator (MOZ)
all <- all %>%
  dplyr::mutate(otherdisaggregate = ifelse(indicator == "3MD", "3 months", otherdisaggregate),
                disaggregate = ifelse(indicator == "3MD", "Period", disaggregate),
                indicator = ifelse(indicator %in% c("3MD", "TX_MMS"), "TX_MMD", indicator))
#adjust age and sex to be unknown for DATIM
all <- all %>%
  dplyr::mutate(sex = ifelse(sex == "Unspecified sex", "Unknown", sex),
                agecoarse = ifelse(agecoarse == "Unknown Age", "Unknown", agecoarse))

all <- all %>%
  dplyr::mutate_at(dplyr::vars(val, mer_results, mer_targets, weekly_targets, targets_gap, weekly_targets_gap), as.numeric) %>%
  dplyr::mutate(agecoarse = ifelse(!is.na(age), age, agecoarse),
                orgunituid = ifelse(!is.na(facilityuid), facilityuid, orgunituid),
                date = lubridate::as_date(date)) %>%
  dplyr::filter(date < "2019-06-10") %>%
  dplyr::select(-c(facilityuid, age, `Pivot1 Names`))

readr::write_tsv(all, paste0("C:/Users/achafetz/Downloads/HFR_ready/HFR_output_", format(Sys.time(),"%Y%m%d.%H%M"), ".txt"), na = "")

dplyr::mutate(df_hfr, date = lubridate::mdy(date))



