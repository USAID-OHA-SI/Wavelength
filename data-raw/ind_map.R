## INDICATOR MAPPING

library(tidyverse)
library(usethis)


# TANZANIA ----------------------------------------------------------------

  #import indicator mapping
    ind_map_tza <- read_csv("data-raw/ind_map_tza.csv")

  #filter all indicators collected to ones that we currently want to review
    ind_map_tza <- filter(ind_map_tza, !is.na(indicator))

  #save to data
    use_data(ind_map_tza, overwrite = TRUE)

# UGANDA ------------------------------------------------------------------

  #import indicator mapping
    ind_map_uga <- read_csv("data-raw/ind_map_uga.csv")

  #save to data
    use_data(ind_map_uga, overwrite = TRUE)
