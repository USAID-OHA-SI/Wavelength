## INDICATOR MAPPING

library(tidyverse)
library(usethis)


# TANZANIA ----------------------------------------------------------------

  #MONTHLY
  #import indicator mapping
    ind_map_tza_old <- read_csv("data-raw/ind_map_tza_old.csv")

  #filter all indicators collected to ones that we currently want to review
    ind_map_tza_old <- filter(ind_map_tza_old, !is.na(indicator))

  #save to data
    use_data(ind_map_tza_old, overwrite = TRUE)

  #WEEKLY
  #import indicator mapping
  ind_map_tza <- read_csv("data-raw/ind_map_tza.csv")

  #filter all indicators collected to ones that we currently want to review
  ind_map_tza <- filter(ind_map_tza, !is.na(indicator))

  #save to data
  use_data(ind_map_tza, overwrite = TRUE)

# UGANDA ------------------------------------------------------------------

  #import indicator mapping
    ind_map_uga <- readr::read_csv("data-raw/ind_map_uga.csv")

  #save to data
    usethis::use_data(ind_map_uga, overwrite = TRUE)

# BURUNDI -----------------------------------------------------------------

  #import indicator mapping
    ind_map_bdi <- readr::read_csv("data-raw/ind_map_bdi.csv")

  #save to data
    use_data(ind_map_bdi, overwrite = TRUE)

# LESOTHO -----------------------------------------------------------------

  #import indicator mapping
  ind_map_lso <- readr::read_csv("data-raw/ind_map_lso.csv",
                                 col_types = readr::cols(.default = "c"))

  #save to data
  usethis::use_data(ind_map_lso, overwrite = TRUE)

# ETHIOPIA ----------------------------------------------------------------

  #import indicator mapping
  ind_map_eth <- readr::read_csv("data-raw/ind_map_eth.csv",
                                 col_types = readr::cols(.default = "c"))

  #save to data
  usethis::use_data(ind_map_eth, overwrite = TRUE)


# KENYA -------------------------------------------------------------------

  #import indicator mapping
  ind_map_ken <- readr::read_csv("data-raw/ind_map_ken.csv",
                                 col_types = readr::cols(.default = "c"))

  #save to data
  usethis::use_data(ind_map_ken, overwrite = TRUE)
