## PROJECT:  HFR
## AUTHOR:   A.CHAFETZ | USAID
## PURPOSE:  resolve sqlview issues


devtools::load_all()
library(tidyverse)
library(vroom)
library(lubridate)
library(glamr)
library(googledrive)

# drive_auth()
#
# gfldr <- "1SgZkdG5uu-Syy6DYsNbTrzDqUmK4fSgF"
#
# (file <- drive_ls(as_id(gfldr), "2020\\.13"))

path <- "out/joint"

(filename <- list.files(path, "HFR_2021.*Tableau", full.names = TRUE) %>% last())

# filename <- drive_download(as_id(file$id), file.path(path, file$name))
#
# filename <- pull(filename, local_path)

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
         hfr_freq = tolower(hfr_freq)
         ) %>%
  filter(date != "2109-11-25",
         fy != 2110)


df_adj %>%
  count(fy, hfr_pd, date)

distinct(df_adj, indicator)
distinct(df_adj, agecoarse)
distinct(df_adj, sex)
distinct(df_adj, otherdisaggregate)
distinct(df_adj, indicator,otherdisaggregate) %>% filter(!otherdisaggregate %in% c("\\N", "nan"))
distinct(df_adj, hfr_freq)


# new_filename <- filename %>%
#   basename() %>%
#   str_replace(".(csv|zip)$", "_adj.csv") %>%
#   file.path("out", "joint", .)

(filename <- list.files(path, "HFR_Tableau_SQLview_FY20", full.names = TRUE))

df_twbx_fy20 <- hfr_read(filename)

df_twbx_fy20 <- df_twbx_fy20 %>%
  rename(hfr_freq = period_type)
df_adj2 <- bind_rows(df_adj, df_twbx_fy20)


df_adj2 %>%
  count(fy, hfr_pd, date)

distinct(df_adj2, indicator)
distinct(df_adj2, agecoarse)
distinct(df_adj2, sex)
distinct(df_adj2, otherdisaggregate)
distinct(df_adj2, indicator,otherdisaggregate) %>% filter(!otherdisaggregate %in% c("\\N", "nan"))
count(df_adj2, hfr_freq)


new_filename <- "out/joint/HFR_Tableau_SQLview.csv"

write_csv(df_adj2, new_filename, na = "")

# drive_put(new_filename, as_id("1onKyOQv3V6H4UPaDCQhH1Y9i8fnOU3HS"))
