## PROJECT:  HFR
## AUTHOR:   A.Chafetz | USAID
## PURPOSE:  identify dupliate mechanism files in a pd folder
## DATE:     2020-04-23



#dependencies
  library(googledrive)
  library(tidyverse)
  library(lubridate)

#authenticate GDrive access
  drive_auth()

#find the id for the HFR data folder
  datafldr_id <-drive_find(pattern = "2. Output") %>%
    pull(id)

#pull the idea for each subsequent period folder to pull each individually
  pd_fldrs <- drive_ls(as_id(datafldr_id), pattern = "2020") %>%
    pull(id)

#pull all files by period folder
  df_files <- map_dfr(.x = pd_fldrs,
                      .f = ~ drive_ls(as_id(.x)) %>%
                        drive_reveal(what = c("path")) %>%
                        distinct(name, path, id) %>%
                        select(path, id))

#extract the folders, mech codes and processed dates from filepath
  df_extract <- df_files %>%
    separate(path, c(NA, "pd_folder", "filename"), sep = "/") %>%
    mutate(mech_code = str_extract(filename, "(?<=_)[:digit:]+(?=_)"),
           date_processed = str_extract(filename, "[:digit:]+(?=\\.csv)") %>%
             ymd) %>%
    select(id, filename, everything())

#flag where there are multiple mechanism in a folder
  df_extract <- df_extract %>%
    group_by(pd_folder, mech_code) %>%
    mutate(count = n()) %>%
    ungroup() %>%
    arrange(pd_folder, mech_code, date_processed)

#mech file count by period
  df_extract %>%
    count(pd_folder, count) %>%
    spread(count, n)

#id keep/drop
  df_dups <- df_extract %>%
    filter(count > 1,
           !is.na(mech_code)) %>%
    group_by(pd_folder, mech_code) %>%
    mutate(status = ifelse(row_number() == max(row_number()), "keep", "drop")) %>%
    ungroup()

#export list for sharing/identifiation
  df_dups %>%
    write_csv("out/HFR_duplicate_processed_files.csv", na = "")

#identify where we want the duplicates to keep to go
  new_fldr <- drive_ls(as_id(datafldr_id), pattern = "DUPLICATE KEEPS") %>%
    pull(id)

#filter the duplicates list to only ones to keep
  df_mv <- df_dups %>%
    filter(status == "keep")

#move files to new folder
  walk2(.x = df_mv$id,
        .y = df_mv$filename,
        .f = ~ drive_cp(as_id(.x), path = as_id(new_fldr), name = .y))

#check that al moved
  drive_ls(as_id(new_fldr))


