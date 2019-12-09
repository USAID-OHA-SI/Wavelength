## ISO Codes

#import indicator mapping
  myuser <- ""
  iso_map <- identify_levels(username = myuser, password = mypwd(myuser))

#list of distinct OUs + ISO codes
  iso_distinct_ous <- dplyr::distinct(iso_map, operatingunit = name3, iso = iso3)

#list of distinct countries under regional programs + ISO codes
  iso_distinct_ctrys <- iso_map %>%
    dplyr::filter(!is.na(iso4)) %>%
    dplyr::distinct(operatingunit = name4, iso = iso4) %>%
    dplyr::mutate(regional = TRUE)

#bind together
  iso_map <- dplyr::bind_rows(iso_distinct_ous, iso_distinct_ctrys) %>%
    dplyr::arrange(operatingunit)

#save to data
  usethis::use_data(iso_map, overwrite = TRUE)
