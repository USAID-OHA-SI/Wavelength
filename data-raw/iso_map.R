## AGE MAPPING

library(tidyverse)
library(usethis)


#import indicator mapping
  iso_map <- read_csv("data-raw/ISOcodes_PEPFAR_Countries.csv")

#save to data
  use_data(iso_map, overwrite = TRUE)
