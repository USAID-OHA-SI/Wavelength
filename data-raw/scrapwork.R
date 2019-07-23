# Pull from DATIM ---------------------------------------------------------



ous <- c("Angola", "Botswana", "Burma", "Burundi", "Cameroon",
         "Cote d'Ivoire", "Democratic Republic of the Congo",
         "Dominican Republic", "Eswatini", "Ethiopia",
         "Ghana", "Haiti", "India", "Indonesia",
         "Kenya", "Lesotho", "Malawi", "Mozambique",
         "Namibia", "Nigeria", "Papua New Guinea", "Rwanda",
         "South Africa", "South Sudan", "Tanzania", "Uganda",
         "Ukraine", "Vietnam", "Zambia", "Zimbabwe")

ous <- c("Zambia", "Zimbabwe")



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

get_em("Malawi")

purrr::walk(ous, ~extract_datim(ou_name = .x,
                                  username = myuser,
                                  password = mypwd(myusername),
                                  folderpath_output = "out/"))



files <- list.files("C:/Users/achafetz/Downloads", "HFR_DATIM", full.names = TRUE)


full_set <- purrr::map_dfr(files, readr::read_tsv, col_types = c(.default = "c"))
folderpath_output <- "C:/Users/achafetz/Downloads"
filename <- paste0("HFR_DATIM_FY19Q2_Global_", format(Sys.Date(),"%Y%m%d"), ".txt")
readr::write_tsv(full_set, file.path(folderpath_output, filename), na = "")





path_datim <- list.files("out/DATIM", "FY19Q2_Zam", full.names = TRUE) %>% fs::path_abs()
path_hfr <- list.files("out/processed", "ZMB", full.names = TRUE)[2] %>% fs::path_abs()

x <- combine_sources(path_hfr, path_datim, "2019-06-10", 10)
x <- combine_sources(path_hfr, path_datim, "2019-05-13", 9)


x2 <- list.files("out/joint", "ZMB", full.names = TRUE) %>%
  purrr::map_dfr(readr::read_tsv, col_type = c(.default = "c"))

x2 %>%
  dplyr::mutate_at(dplyr::vars(mer_results, mer_targets, weekly_targets,
                               targets_gap, weekly_targets_gap, val), as.numeric) %>%
  dplyr::group_by_if(is.character) %>%
  dplyr::summarise_if(is.numeric, sum, na.rm = TRUE) %>%
  dplyr::ungroup() %>%
  readr::write_tsv("C:/Users/achafetz/Documents/GitHub/Wavelength/out/Processed/HFR_JOINT_ZMB_FY19P09-10.txt", na = "")


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



