## PROJECT:  HFR
## AUTHOR:   A.CHAFETZ | USAID
## PURPOSE:  OU completeness report
## DATE:     2020-09-23
## UPDATED:
## NOTE:     adapted from completeness_report

# DEPENDENCIES ------------------------------------------------------------

  library(tidyverse)
  library(scales)
  library(extrafont)
  library(glitr)
  library(ICPIutilities)
  library(patchwork)
  library(ggrepel)
  library(RColorBrewer)
  devtools::load_all()

# VARIABLES ---------------------------------------------------------------

  #global file path
    fldr_path <- "../pump_up_the_jam/Data"

  #red
    flag_red <- brewer.pal(5, "OrRd")[5]

# IMPORT DATASET ----------------------------------------------------------

    # Load data from most recent to least recent; Filtering as needed
    hfr_pattern = c("08242020|08122020|06242020")
    hfr_views <- list.files(path = fldr_path, pattern = hfr_pattern, full.names = TRUE)


    # Load periods incrementally b/c some have to be filtered.
    tmp <- map(hfr_views, ~hfr_read(.x))

    hfr_all <- bind_rows(tmp[[4]] %>% as_tibble(),
                         tmp[[3]] %>% as_tibble() %>% filter(hfr_pd != 9),
                         tmp[[2]] %>% as_tibble(),
                         tmp[[1]] %>% as_tibble())



# MUNGE -------------------------------------------------------------------

  df_glob <- hfr_all

  #add iso codes before adjusting OU name
    df_glob <- left_join(df_glob, iso_map)

  #rename OUs
    df_glob <-  df_glob %>%
      filter(operatingunit != "\\N") %>%
      dplyr::mutate(operatingunit = recode(operatingunit,
                                           "Democratic Republic of the Congo" ="DRC",
                                           "Dominican Republic" = "DR",
                                           "West Africa Region" = "WAR",
                                           "Western Hemisphere Region" = "WHR"
                                           ))

# AGGREGATE ---------------------------------------------------------------

  #aggregate sites total (agg age/sex or months) to preserve distinct sites
    df_glob_site_agg <- df_glob %>%
      group_by(operatingunit, iso, orgunituid, hfr_pd, date, indicator) %>%
      summarise_at(vars(mer_targets, val), sum, na.rm = TRUE) %>%
      ungroup()

  #aggregate pd total for non-cumulative indicators
    df_glob_wk_agg <- df_glob_site_agg %>%
      filter(!indicator %in% c("TX_CURR", "TX_MMD")) %>%
      mutate(target_sitecnt = ifelse(mer_targets > 0, 1, 0),
             hfr_sitecnt = ifelse(val > 0, 1, 0)) %>%
      group_by(operatingunit, iso, indicator, hfr_pd, date) %>%
      summarise_at(vars(mer_targets, val, target_sitecnt, hfr_sitecnt), sum, na.rm = TRUE) %>%
      ungroup()

  #agg max site value for cumulative indicators
    df_glob_site_txcurr_agg <- df_glob_site_agg %>%
      filter(indicator %in% c("TX_CURR", "TX_MMD")) %>%
      group_by(operatingunit, iso, orgunituid, hfr_pd, indicator) %>%
      summarise_at(vars(mer_targets, val), max, na.rm = TRUE) %>%
      ungroup()

  #create TX_MMD pseudo target target
    df_glob_site_mmdtarg <- df_glob_site_txcurr_agg %>%
      filter(indicator == "TX_CURR") %>%
      select(-val) %>%
      mutate(indicator = "TX_MMD")

  #append mmd targets on, create site counts and agg to pd
    df_glob_site_txcurr_agg <- df_glob_site_txcurr_agg %>%
      bind_rows(df_glob_site_mmdtarg) %>%
      mutate(target_sitecnt = ifelse(mer_targets > 0, 1, 0),
             hfr_sitecnt = ifelse(val > 0, 1, 0)) %>%
      group_by(operatingunit, iso, indicator, hfr_pd) %>%
      summarise_at(vars(mer_targets, val, target_sitecnt, hfr_sitecnt), sum, na.rm = TRUE) %>%
      ungroup()

  #combine cumulative + non cumulative
    df_glob_agg <- bind_rows(df_glob_wk_agg, df_glob_site_txcurr_agg)

    rm(df_glob_wk_agg, df_glob_site_agg, df_glob_site_mmdtarg, df_glob_site_txcurr_agg)

# SUMMARIZE COMPLETENESS --------------------------------------------------

  #completeness
    df_glob_agg <- df_glob_agg %>%
      mutate(completeness = hfr_sitecnt/target_sitecnt,
             completeness = ifelse(is.infinite(completeness), NA, completeness))

    df_glob_agg <- df_glob_agg %>%
      filter(!date %in% c("2109-11-25"))

  #heatmap by country x indicator
    df_viz <- df_glob_agg %>%
      dplyr::mutate(tx_curr_tgts = case_when(indicator == "TX_CURR" ~ mer_targets),
                    operatingunit = fct_reorder(operatingunit, tx_curr_tgts, sum, na.rm = TRUE),
                    # hfr_pd = str_sub(hfr_pd, -2),
                    indicator = factor(indicator, c("TX_CURR", "TX_MMD",
                                                    "HTS_TST", "HTS_TST_POS",
                                                    "TX_NEW", "VMMC_CIRC", "PrEP_NEW"
                                                    )),
                    completeness = ifelse(is.infinite(completeness), NA, completeness),
                    comp_bin = case_when(is.na(completeness) ~ NA_character_,
                                         completeness == 0 ~ "1",
                                         completeness <= .25 ~ "2",
                                         completeness <= .5 ~ "3",
                                         completeness <= .75 ~ "4",
                                         TRUE ~ "5"
                                         ))


# PLOT --------------------------------------------------------------------


    plot_wk <- function(ind){
      v <- df_viz %>%
        filter(indicator == {{ind}}) %>%
        group_by(hfr_pd) %>%
        mutate(hfr_pd_date = min(date) %>% format(., "%b %d")) %>%
        ungroup() %>%
        mutate(hfr_pd_date = fct_reorder(hfr_pd_date, date)) %>%
        ggplot(aes(as.character(date), operatingunit, fill = comp_bin)) +
        geom_tile(color = "white") +
        # geom_text(aes(label = ifelse(completeness <= .5, percent(completeness,1), NA),
        #               color = completeness > .75), na.rm = TRUE,
        #           family = "Source Sans Pro", size = 2.5) +
        facet_grid(~ hfr_pd_date, scales = "free_x") +
        scale_fill_brewer(palette = "OrRd", direction = -1) +
        scale_color_manual(values = c("gray10", "gray50")) +
        scale_x_discrete(position = "top") +
        labs(x = NULL, y = NULL,
             title = paste0("WEEKLY HFR COMPLETENESS | ",{{ind}}),
             # subtitle = "mechanism x site completeness",
             caption = "data as of HFR 2020.11 [2020-08-24]") +
        si_style_nolines() +
        theme(legend.position = "none",
              panel.spacing = unit(0, "lines"),
              axis.text.x = element_blank())
      ggsave(file.path("out", paste0("WklyCompl_", {{ind}}, ".png")),
             dpi = 330, height = 5.625, width = 10)
      return(v)
    }

    plot_wk("TX_NEW")
    plot_wk("HTS_TST_POS")
    plot_wk("VMMC_CIRC")
    plot_wk("PrEP_NEW")


date_map <- df_viz %>%
  distinct(date, hfr_pd) %>%
  filter(!is.na(date)) %>%
  group_by(hfr_pd) %>%
  filter(date == min(date)) %>%
  ungroup() %>%
  rename(date_min = date)

    plot_pd <- function(ind){
      v <- df_viz %>%
        filter(indicator == {{ind}}) %>%
        left_join(date_map, by = "hfr_pd") %>%
        mutate(hfr_pd = str_pad(hfr_pd, 2, pad = "0"),
               date = format(date_min, "%b %d"),
               date = fct_reorder(date, date_min)) %>%
        ggplot(aes(date, operatingunit, fill = comp_bin)) +
        geom_tile(color = "white") +
        # geom_text(aes(label = ifelse(completeness <= .5, percent(completeness,1), NA),
        #               color = completeness > .75), na.rm = TRUE,
        #           family = "Source Sans Pro", size = 2.5) +
        facet_wrap(~ indicator) %>%
        scale_fill_brewer(palette = "OrRd", direction = -1) +
        scale_color_manual(values = c("gray10", "gray50")) +
        scale_x_discrete(position = "top") +
        labs(x = NULL, y = NULL,
             title = paste0("PERIOD HFR COMPLETENESS | ", {{ind}}),
             # subtitle = "mechanism x site completeness",
             caption = "data as of HFR 2020.11 [2020-08-24]") +
        si_style_nolines() +
        theme(legend.position = "none")
      ggsave(file.path("out", paste0("PdCompl_", {{ind}}, ".png")),
             dpi = 330, height = 5.625, width = 10)
      return(v)
    }

    plot_pd("TX_CURR")
    plot_pd("TX_MMD")


