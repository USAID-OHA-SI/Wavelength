## PROJECT:  HFR
## AUTHOR:   A.CHAFETZ | USAID
## PURPOSE:  resolve sqlview issues


devtools::load_all()
library(tidyverse)
library(vroom)
library(lubridate)
library(glamr)
library(googledrive)

drive_auth()

file <- drive_ls(as_id("1SgZkdG5uu-Syy6DYsNbTrzDqUmK4fSgF"), "2020.10")

path <- "C:/Users/achafetz/Downloads/"

filename <- drive_download(as_id(file$id), file.path(path, file$name))

filename <- pull(filename, local_path)

df_twbx <- hfr_read(filename)

glimpse(df_twbx)

df_twbx %>%
  distinct(fy, hfr_pd, date) %>%
  arrange(fy, date) %>%
  print(inf)

df_twbx %>%
  count(fy, hfr_pd, date, wt = val) %>%
  arrange(fy, date) %>%
  print(inf)

df_twbx %>%
  group_by(fy, hfr_pd, date) %>%
  summarise_at(vars(val, mer_results, mer_targets), sum, na.rm = TRUE) %>%
  ungroup() %>%
  arrange(fy, date) %>%
  print(inf)


distinct(df_twbx, indicator)
distinct(df_twbx, agecoarse)
distinct(df_twbx, sex)
distinct(df_twbx, otherdisaggregate)
distinct(df_twbx, indicator,otherdisaggregate) %>% filter(!otherdisaggregate %in% c("\\N", "nan"))

df_twbx %>%
  filter(indicator == "TX_CURR",
         !otherdisaggregate %in% c("\\N", "nan", NA)) %>%
  distinct(hfr_pd, operatingunit, otherdisaggregate)

df_twbx %>%
  filter(operatingunit == "Mozambique") %>%
  count(hfr_pd, indicator, wt = val)

df_twbx %>%
  filter(operatingunit == "Mozambique",
         indicator == "TX_CURR") %>%
  count(hfr_pd, indicator, otherdisaggregate, wt = val)


df_adj <- df_twbx %>%
  mutate(primepartner = str_remove(primepartner, "\r$"),
         sex = ifelse(!sex %in% c("Male", "Female"), "Unspecified", sex),
         agecoarse = ifelse(!agecoarse %in% c("<15", "15+"), "Unknown", agecoarse),
         otherdisaggregate = ifelse(otherdisaggregate == "< 3 months", "<3 months", otherdisaggregate),
         otherdisaggregate = str_replace(otherdisaggregate, "Months", "months"),
         otherdisaggregate = str_replace(otherdisaggregate, "(3|6)m", "\\1 m"),
         otherdisaggregate = ifelse(otherdisaggregate == "6 months", "6 months or more", otherdisaggregate),
         otherdisaggregate = na_if(otherdisaggregate, "0.0"),
         otherdisaggregate = na_if(otherdisaggregate, "nan"),
         otherdisaggregate = na_if(otherdisaggregate, "\\N"),
         otherdisaggregate = na_if(otherdisaggregate, "Positivo"),
         otherdisaggregate = na_if(otherdisaggregate, "Total"),
         psnuuid = NA_character_) %>%
  filter(date != "2109-11-25",
         fy != 2110)


df_adj %>%
  count(fy, hfr_pd, date)

distinct(df_adj, indicator)
distinct(df_adj, agecoarse)
distinct(df_adj, sex)
distinct(df_adj, otherdisaggregate)
distinct(df_adj, indicator,otherdisaggregate) %>% filter(!otherdisaggregate %in% c("\\N", "nan"))


# new_filename <- filename %>%
#   basename() %>%
#   str_replace(".(csv|zip)$", "_adj.csv") %>%
#   file.path("out", "joint", .)

new_filename <- "out/joint/HFR_Tableau_SQLview.csv"

write_csv(df_adj, new_filename, na = "")

# drive_put(new_filename, as_id("1onKyOQv3V6H4UPaDCQhH1Y9i8fnOU3HS"))
