## AGE MAPPING

library(tidyverse)
library(usethis)


#import indicator mapping
  age_map <- read_csv("data-raw/age_map.csv")

#save to data
  use_data(age_map, overwrite = TRUE)
