## PROJECT:  HFR
## AUTHOR:   A.CHAFETZ | USAID
## PURPOSE:  OU completeness report
## DATE:     2019-10-03
## UPDATED:  2020-06-02


# DEPENDENCIES ------------------------------------------------------------

  library(tidyverse)
  library(scales)
  library(extrafont)
  library(glitr)
  library(ICPIutilities)
  library(patchwork)
  library(ggrepel)
  devtools::load_all()

# VARIABLES ---------------------------------------------------------------

  #global file path
    path <- "out/joint/HFR_2020.07_Tableau_20200518_adj.csv"
    # path_new <- "out/joint/HFR_GLOBAL_2020.02_output_20200107.0838.csv"

  #numeric variables to convert from string
    vars_num <- c("mer_results", "mer_targets", "targets_gap", "weekly_targets", "weekly_targets_gap", "val")

  #list of OUs to review
    ou_lst <- c("Botswana", "Burundi", "Cameroon", "Cote d'Ivoire",
                "Democratic Republic of the Congo", "Dominican Republic",
                "Eswatini", "Ethiopia", "Haiti", "Kenya", "Lesotho",
                "Malawi", "Mozambique", "Namibia", "Nigeria", "South Africa",
                "South Sudan", "Tanzania", "Uganda", "Ukraine", "Vietnam",
                "Zambia", "Zimbabwe")

# IMPORT DATASET ----------------------------------------------------------

  #import, reading in all as character as default
    df_glob <- hfr_read(path)
  #import new
    # df_glob_new <- hfr_read(path_new)
  #append
    # df_glob <- dplyr::bind_rows(df_glob, df_glob_new)
    # rm(df_glob_new)

# MUNGE -------------------------------------------------------------------

  #adjust numeric variables and fix issue with lines w/ HTS_TST POS
    # df_glob <- df_glob %>%
    #   dplyr::mutate_at(dplyr::vars(vars_num), as.numeric) %>%
    #   dplyr::mutate_at(dplyr::vars(fy, hfr_pd), as.integer)

  #adjust period
    df_glob <- dplyr::mutate(df_glob, hfr_pd = fy + (hfr_pd/100))

  #filter out PD 8 (not officially collected) & restrict to OU's in list; shorten to DRC
    df_glob <-  df_glob %>%
      # dplyr::filter(hfr_pd > 2019.08,
      #               operatingunit %in% ou_lst
      #               ) %>%
      dplyr::filter(operatingunit != "\\N") %>%
      dplyr::mutate(operatingunit = recode(operatingunit,
                                           "Democratic Republic of the Congo" ="DRC",
                                           "Dominican Republic" = "DR",
                                           "West Africa Region" = "WAR",
                                           "Western Hemisphere Region" = "WHR"
                                           ))

  #create denom for TX_MMD
    df_glob_mmd <- df_glob %>%
      dplyr::filter(indicator == "TX_CURR") %>%
      dplyr::group_by(operatingunit, snu1, psnu, orgunit, mech_code, hfr_pd, indicator) %>% #orgunituid
      dplyr::summarise_at(dplyr::vars(mer_targets), sum, na.rm = TRUE) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(indicator = "TX_MMD")


# SUMMARIZE COMPLETENESS --------------------------------------------------

  #append MMD "target" sites
    df_glob_sites <- dplyr::bind_rows(df_glob, df_glob_mmd)

  #distinct set of sites w/ their MER targets and HFR values
    df_glob_sites <- df_glob_sites %>%
      dplyr::group_by(operatingunit, snu1, psnu, orgunit, mech_code, hfr_pd, indicator) %>% #orgunituid
      dplyr::summarise_at(dplyr::vars(mer_targets, val), sum, na.rm = TRUE) %>%
      dplyr::ungroup()

  #create site count for targets & HFR reporting
    df_glob_sites_comp <- df_glob_sites %>%
      dplyr::mutate(target_sitecnt = ifelse(mer_targets > 0, 1, 0),
                    hfr_sitecnt = ifelse(val > 0, 1, 0)) %>%
      dplyr::group_by(operatingunit, mech_code, indicator, hfr_pd) %>%
      dplyr::summarise_at(dplyr::vars(mer_targets, val, target_sitecnt, hfr_sitecnt), sum, na.rm = TRUE) %>%
      dplyr::ungroup()

  #record the distinct site count for each OUxIndicator
    df_glob_sites_n <- df_glob_sites_comp %>%
      dplyr::group_by(operatingunit, indicator) %>%
      dplyr::summarise_at(dplyr::vars(target_sitecnt), max, na.rm = TRUE) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(indicator = paste0(indicator, "_n")) %>%
      dplyr::rename(completeness = target_sitecnt)

  #create completeness metric
    df_glob_sites_comp <- df_glob_sites_comp %>%
      dplyr::mutate(completeness = hfr_sitecnt/target_sitecnt)

  #clean for export to Excel (clipr - value to clipboard)
    df_glob_sites_comp %>%
      dplyr::filter(hfr_pd >= 2019.12) %>%
      dplyr::select(-c(mer_targets, val, target_sitecnt, hfr_sitecnt)) %>%
      dplyr::arrange(operatingunit, indicator, hfr_pd) %>%
      tidyr::unite("indicator", c("indicator", "hfr_pd"), sep = "_pd") %>%
      dplyr::bind_rows(df_glob_sites_n) %>%
      tidyr::spread(indicator, completeness) %>%
      dplyr::mutate_at(dplyr::vars(dplyr::ends_with("_n")), ~ dplyr::na_if(., 0)) %>%
      dplyr::select(operatingunit, dplyr::starts_with("HTS_TST"), dplyr::starts_with("TX_NEW"),
                    dplyr::starts_with("VMMC_CIRC"), dplyr::starts_with("PrEP_NEW"),
                    dplyr::starts_with("TX_CURR"), dplyr::starts_with("TX_MMD"))


  #heatmap by country x indicator
    df_glob_sites_comp %>%
      dplyr::mutate(tx_curr_tgts = case_when(indicator == "TX_CURR" ~ mer_targets),
                    operatingunit = fct_reorder(operatingunit, tx_curr_tgts, sum, na.rm = TRUE),
                    hfr_pd = str_sub(hfr_pd, -2),
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
                                         )) %>%
      ggplot(aes(hfr_pd, operatingunit, fill = comp_bin)) +
      geom_tile(color = "white") +
      geom_text(aes(label = percent(completeness,1), color = completeness > .75),
                family = "Source Sans Pro", size = 2.5) +
      facet_grid(~ indicator) +
      scale_fill_brewer(palette = "OrRd", direction = -1) +
      scale_color_manual(values = c("gray10", "gray50")) +
      labs(x = NULL, y = NULL,
           title = "LARGE REPORTING GAPS STILL EXISTS FOR KEY INDICATORS LIKE TX_CURR AND MMD",
           subtitle = "mechanism x site completeness of reporting by period",
           caption = "data as of HFR 2020.07 [2020-05-18]") +
      si_style_nolines() +
      #coord_fixed(ratio = 1) +
      theme(legend.position = "none",
            #panel.spacing = unit(.5, "lines"),
            strip.text = element_text(face = "bold"))

    ggsave("out/Completeness050607.png", dpi = 330, height = 5.625, width = 10)



   df_viz <- df_glob_sites_comp %>%
      filter(indicator == "TX_CURR") %>%
      dplyr::mutate(tx_curr_tgts = case_when(indicator == "TX_CURR" ~ mer_targets),
                    completeness = ifelse(is.infinite(completeness), NA, completeness))


   df_avgcomp <-  df_viz %>%
      filter(hfr_pd %in% c("2020.05", "2020.06")) %>%
      group_by(mech_code) %>%
      summarise(avg_comp_2prior = mean(completeness, na.rm = TRUE)) %>%
      ungroup() %>%
      filter(!is.na(avg_comp_2prior))

   df_viz <- df_viz %>%
     filter(hfr_pd == "2020.07") %>%
     left_join(df_avgcomp) %>%
     mutate(comp_prior_bin = case_when(is.na(avg_comp_2prior) ~ NA_character_,
                                       avg_comp_2prior == 0 ~ "0%",
                                       avg_comp_2prior <= (1/3) ~ "<33%",
                                       avg_comp_2prior <= (2/3) ~ "<66%",
                                       TRUE ~ ">66%"
                    ),
            comp_prior_bin = factor(comp_prior_bin, c("0%","<33%", "<66%", ">66%")))

   df_viz <- rename_official(df_viz)

   df_viz <- left_join(df_viz, iso_map)


   #plot completened by PD
   # df_viz %>%
   #   filter(indicator == "TX_CURR") %>%
   #   mutate(completeness = ifelse(completeness > 1, 1, completeness)) %>%
   #   ggplot(aes(tx_curr_tgts, completeness, fill = completeness)) +
   #   geom_point(size = 4, alpha = .5, shape = 21, stroke = 1, color = "gray70") +
   #   facet_grid(indicator ~ hfr_pd) +
   #   scale_x_log10(label = comma) +
   #   scale_y_continuous(label = percent) +
   #   scale_fill_distiller(palette = "OrRd", direction = -1, na.value = "grey50") +
   #   labs(x = "FY20 TX_CURR MER TARGETS", y = "HFR Site Reporting Completeness",
   #        caption = "HFR Data [2020-05-20]") +
   #   si_style() +
   #   theme(legend.position = "none",
   #         strip.text = element_text(face = "bold"))


   #plot completeness pd 7
    v1 <- df_viz %>%
      mutate(completeness = ifelse(completeness > 1, 1, completeness)) %>%
      filter(!is.na(comp_prior_bin)) %>%
      ggplot(aes(mer_targets, completeness, fill = comp_prior_bin)) +
      geom_point(size = 4, alpha = .5, shape = 21, stroke = 1, color = "gray70") +
      scale_x_log10(label = comma) +
      scale_y_continuous(label = percent) +
      scale_fill_brewer(palette = "OrRd", direction = -1, na.value = "grey50",
                        name = "Last 2 Pd Avg. Site Completeness") +
      labs(x = "FY20 TX_CURR MER TARGETS", y = "HFR Site Reporting Completeness",
           subtitle = "HFR 2020.07 | TX_CURR",
           caption = "completeness capped at 100%; historic completeness is average of last two periods
           HFR Data [2020-05-20]") +
      si_style() +
      theme(strip.text = element_text(face = "bold"),
            legend.title = element_text(family = "Source Sans Pro", color = "gray30"))

  #create df for inset focusing on <25% completeness and targets larger than 100k
    df_viz_inset <- df_viz %>%
      mutate(completeness = ifelse(completeness > 1, 1, completeness)) %>%
      filter(!is.na(comp_prior_bin),
             completeness <= .25,
             mer_targets >= 100000) %>%
      mutate(
        primepartner = case_when(primepartner == "Elizabeth Glaser Pediatric Aids Foundation" ~ "EGPAF",
                                      str_detect(primepartner, "Baylor") ~ "Baylor",
                                      primepartner == "Partners In Hope" ~ "PIH",
                                      str_detect(primepartner, "TBD") ~ "TBD",
                                      primepartner == "KHETHIMPILO AIDS FREE LIVING" ~ "KI",
                                      primepartner == "John Snow, Incorporated" ~ "JSI",
                                      primepartner == "JHPIEGO CORPORATION" ~ "JHPIEGO",
                                      primepartner == "Family Health International" ~ "FHI360",
                                      str_detect(primepartner, "ORGANIZATION FOR PUBLIC HEALTH") ~ "OPHID"),
             # lab = paste0(iso, " ", mech_code, ": ", primepartner))
             lab = paste0(iso, ": ", primepartner))

  #plot inset
    v2 <- df_viz_inset %>%
      ggplot(aes(mer_targets, completeness, fill = comp_prior_bin)) +
      geom_point(size = 4, alpha = .5, shape = 21, stroke = 1, color = "gray70") +
      geom_text_repel(aes(label = lab), family = "Source Sans Pro", size = 3,
                      color = "gray40", force = 30) +
      scale_x_log10(label = comma) +
      scale_y_continuous(label = percent_format(1)) +
      scale_fill_brewer(palette = "OrRd", direction = -1, na.value = "grey50",
                        name = "Historic Avg. Site Completeness") +
      expand_limits(y = -.05) +
      labs(x = NULL, y = NULL) +
      si_style() +
      theme(legend.position = "none")

  #combine plots
    v1 | (v2 / plot_spacer())

  #export
    ggsave("out/CompletenessMechs07.png", dpi = 330, height = 5.625, width = 10)

  #share of tx target portfolio with no targets
   df_viz %>%
     mutate(no_rep = completeness == 0) %>%
     count(no_rep, wt = mer_targets) %>%
     mutate(cum_share = n / sum(n))


