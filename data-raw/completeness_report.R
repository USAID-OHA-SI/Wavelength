## PROJECT:  HFR
## AUTHOR:   A.CHAFETZ | USAID
## PURPOSE:  OU completeness report
## DATE:     2019-10-03
## UPDATED:  2020-06-04


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
    path <- "out/joint/HFR_2020.07_Tableau_20200518_adj.csv"
    # path_new <- "out/joint/HFR_GLOBAL_2020.02_output_20200107.0838.csv"

  #red
    flag_red <- brewer.pal(5, "OrRd")[5]

# IMPORT DATASET ----------------------------------------------------------

  #import, reading in all as character as default
    df_glob <- hfr_read(path)
  #import new
    # df_glob_new <- hfr_read(path_new)
  #append
    # df_glob <- dplyr::bind_rows(df_glob, df_glob_new)
    # rm(df_glob_new)

# MUNGE -------------------------------------------------------------------

  #adjust period
    df_glob <- mutate(df_glob, hfr_pd = fy + (hfr_pd/100))

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
      group_by(operatingunit, iso, orgunituid, mech_code, hfr_pd, date, indicator) %>%
      summarise_at(vars(mer_targets, val), sum, na.rm = TRUE) %>%
      ungroup()

  #aggregate pd total for non-cumulative indicators
    df_glob_pd_agg <- df_glob_site_agg %>%
      filter(!indicator %in% c("TX_CURR", "TX_MMD")) %>%
      mutate(target_sitecnt = ifelse(mer_targets > 0, 1, 0),
             hfr_sitecnt = ifelse(val > 0, 1, 0)) %>%
      group_by(operatingunit, iso, mech_code, indicator, hfr_pd) %>%
      summarise_at(vars(mer_targets, val, target_sitecnt, hfr_sitecnt), sum, na.rm = TRUE) %>%
      ungroup()

  #agg max site value for cumulative indicators
    df_glob_site_txcurr_agg <- df_glob_site_agg %>%
      filter(indicator %in% c("TX_CURR", "TX_MMD")) %>%
      group_by(operatingunit, iso, orgunituid, mech_code, hfr_pd, indicator) %>%
      summarise_at(vars(mer_targets, val), max, na.rm = TRUE) %>%
      ungroup()

  #create TX_MMD pseudo target target
    df_glob_site_mmdtarg <- df_glob_site_txcurr_agg %>%
      filter(indicator == "TX_CURR") %>%
      select(-val) %>%
      mutate(indicator == "TX_MMD")

  #append mmd targets on, create site counts and agg to pd
    df_glob_site_txcurr_agg <- df_glob_site_txcurr_agg %>%
      bind_rows(df_glob_site_mmdtarg) %>%
      mutate(target_sitecnt = ifelse(mer_targets > 0, 1, 0),
             hfr_sitecnt = ifelse(val > 0, 1, 0)) %>%
      group_by(operatingunit, iso, mech_code, indicator, hfr_pd) %>%
      summarise_at(vars(mer_targets, val, target_sitecnt, hfr_sitecnt), sum, na.rm = TRUE) %>%
      ungroup()

  #combine cumulative + non cumulative
    df_glob_agg <- bind_rows(df_glob_pd_agg, df_glob_site_txcurr_agg)

    rm(df_glob_pd_agg, df_glob_site_agg, df_glob_site_mmdtarg, df_glob_site_txcurr_agg)

# SUMMARIZE COMPLETENESS --------------------------------------------------


  #heatmap by country x indicator
    # df_glob_agg %>%
    #   dplyr::mutate(tx_curr_tgts = case_when(indicator == "TX_CURR" ~ mer_targets),
    #                 operatingunit = fct_reorder(operatingunit, tx_curr_tgts, sum, na.rm = TRUE),
    #                 hfr_pd = str_sub(hfr_pd, -2),
    #                 indicator = factor(indicator, c("TX_CURR", "TX_MMD",
    #                                                 "HTS_TST", "HTS_TST_POS",
    #                                                 "TX_NEW", "VMMC_CIRC", "PrEP_NEW"
    #                                                 )),
    #                 completeness = ifelse(is.infinite(completeness), NA, completeness),
    #                 comp_bin = case_when(is.na(completeness) ~ NA_character_,
    #                                      completeness == 0 ~ "1",
    #                                      completeness <= .25 ~ "2",
    #                                      completeness <= .5 ~ "3",
    #                                      completeness <= .75 ~ "4",
    #                                      TRUE ~ "5"
    #                                      )) %>%
    #   ggplot(aes(hfr_pd, operatingunit, fill = comp_bin)) +
    #   geom_tile(color = "white") +
    #   geom_text(aes(label = percent(completeness,1), color = completeness > .75),
    #             family = "Source Sans Pro", size = 2.5) +
    #   facet_grid(~ indicator) +
    #   scale_fill_brewer(palette = "OrRd", direction = -1) +
    #   scale_color_manual(values = c("gray10", "gray50")) +
    #   labs(x = NULL, y = NULL,
    #        title = "LARGE REPORTING GAPS STILL EXISTS FOR KEY INDICATORS LIKE TX_CURR AND MMD",
    #        subtitle = "mechanism x site completeness of reporting by period",
    #        caption = "data as of HFR 2020.07 [2020-05-18]") +
    #   si_style_nolines() +
    #   #coord_fixed(ratio = 1) +
    #   theme(legend.position = "none",
    #         #panel.spacing = unit(.5, "lines"),
    #         strip.text = element_text(face = "bold"))
    #
    # ggsave("out/Completeness050607.png", dpi = 330, height = 5.625, width = 10)



   df_viz <- df_glob_agg %>%
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
                                       TRUE ~ ">66%"),
            comp_prior_bin = factor(comp_prior_bin, c("0%","<33%", "<66%", ">66%")),
            flag_grp = completeness <= .25 & mer_targets >= 100000) %>%
     filter(!is.na(comp_prior_bin))

   df_viz <- rename_official(df_viz)


   #partner names for aligning parnters
   df_viz <- tribble(
                                                      ~primepartner,                             ~partner_shortname,
                                              "Abt Associates Inc.",                                          "Abt",
                                           "ANOVA HEALTH INSTITUTE",                                        "ANOVA",
         "BAYLOR COLLEGE OF MEDICINE CH ILDREN FOUNDATION TANZANIA",                                       "Baylor",
          "BAYLOR COLLEGE OF MEDICINE CH ILDRENS FOUNDATION MALAWI",                                       "Baylor",
       "Baylor College of Medicine Children's Foundation - Lesotho",                                       "Baylor",
                          "BEZA POSTERITY DEVELOPMENT ORGANIZATION",                     "Beze Posterity Dev. Org.",
                                  "BROADREACH HEALTHCARE (PTY) LTD",                                   "Broadreach",
                                   "Caris Foundation International",                  "Caris Foundation Internat'l",
                                    "Chemonics International, Inc.",                                    "Chemonics",
                            "CHILDREN OF GOD RELIEF INSTIT UTE LTD",             "Children of God Relief Institute",
                                      "DELOITTE CONSULTING LIMITED",                                     "Deloitte",
                       "Elizabeth Glaser Pediatric Aids Foundation",                                        "EGPAF",
                                      "Family Health International",                                       "FHI360",
                                          "FHI Development 360 LLC",                                       "FHI360",
                                                "Fondation Serovie",                            "Fondation Serovie",
                                       "HEALTH THROUGH WALLS, INC.",                         "Health Through Walls",
                                       "HEARTLAND ALLIANCE LTD-GTE",                                     "Heatland",
                        "HIWOT INTEGRATED DEVELOPMENT ORGANIZATION",                   "Hiwot Integrated Dev. Org.",
                             "Interchurch Medical Assistance, Inc.",               "Interchurch Medical Assistance",
                                  "INTRAHEALTH INTERNATIONAL, INC.",                                  "Intrahealth",
                                              "JHPIEGO CORPORATION",                                      "Jhpiego",
                                          "John Snow, Incorporated",                                          "JSI",
                        "JSI Research And Training Institute, INC.",                                          "JSI",
                                     "KHETHIMPILO AIDS FREE LIVING",                                           "KI",
                               "MOI TEACHING AND REFERRAL HOSPITAL",           "MOI Teaching and Referral Hospital",
     "ORGANIZATION FOR PUBLIC HEALTH INTERVENTIONS AND DEVELOPMENT", "Org. for Public Heath Interventions and Dev.",
      "PAKACHERE INSTITUTE OF HEALTH AND DEVELOPMENT COMMUNICATION",                                    "Pakachere",
                                                 "Partners In Hope",                             "Partners in Hope",
                                                             "PATH",                                         "PATH",
                                         "Pathfinder International",                     "Pathfinder International",
                                "Population Services International",                                          "PSI",
                                        "POPULATION SERVICES KENYA",                                          "PSI",
                                                    "RIGHT TO CARE",                                "Right To Care",
                                        "SOCIETY FOR FAMILY HEALTH",                    "Society for Family Health",
                                                  "TBD (000000000)",                                          "TBD",
                                    "THE LUKE COMMISSION SWAZILAND",                          "The Luke Commission",
                "UNAIDS JOINT UNITED NATIONS PROGRAMME ON HIV/AIDS",                                       "UNAIDS",
                                     "University Research Co., LLC",                                          "URC",
                                 "WITS HEALTH CONSORTIUM (PTY) LTD",                                         "Wits"
     ) %>%
     left_join(df_viz, .)

   #partner completeness
    df_viz <- df_viz %>%
      group_by(partner_shortname) %>%
      mutate(prtnr_comp = sum(hfr_sitecnt) / sum(target_sitecnt)) %>%
      ungroup()

   #plot completeness pd 7
   target_max <- df_viz %>% filter(completeness < 0.01) %>% summarise(max = max(mer_targets, na.rm = TRUE)) %>% pull()

   v1 <- df_viz %>%
     mutate(completeness = ifelse(completeness > 1, 1, completeness),
            filter_greys = if_else(!is.na(comp_prior_bin) & completeness <= 0.25 & mer_targets>= 1e5, "#CB181D", grey40k)) %>%
     filter(!is.na(comp_prior_bin)) %>%
     ggplot() +
     annotate(geom = "rect",
              xmin = 1e5, ymin = -0.03,
              xmax = target_max + 1000000, ymax = 0.23,
              fill = grey30k, alpha = 0.25,
              color = grey50k, linetype = "dashed") +
     geom_point(aes(mer_targets, completeness, fill = filter_greys),
                size = 4, alpha = .75, shape = 21, stroke = 1, color = "white") +
     scale_x_log10(label = comma) +
     scale_y_continuous(label = percent) +
     scale_fill_identity() +
     labs(x = "FY20 TX_CURR MER TARGETS", y = "HFR Site Reporting Completeness",
          subtitle = "HFR 2020.07 | TX_CURR",
          caption = "completeness capped at 100%; historic completeness is average of last two periods
           HFR Data [2020-05-20]") +
     si_style() +
     theme(strip.text = element_text(face = "bold"),
           legend.title = element_text(family = "Source Sans Pro", color = "gray30"))

   v1

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
     ggplot(aes(mer_targets, completeness)) +
     geom_point(fill = "#CB181D", alpha = .75, shape = 21, stroke = 1, color = "white", size = 6) +
     # geom_text_repel(aes(label = lab), family = "Source Sans Pro", size = 3,
     #   color = "gray40", force = 30) +
     scale_x_log10(label = comma) +
     scale_y_continuous(label = percent_format(1), limits = c(-0.03, .23)) +
     scale_fill_identity() +
     expand_limits(y = -.05) +
     labs(x = NULL, y = NULL) +
     si_style() +
     theme(legend.position = "none")
   v2

   #combine plots
   v1 | (v2 / plot_spacer())


  #export
    ggsave("out/CompletenessMechs07.png", dpi = 600, height = 4.78, width = 9.59)

  #share of tx target portfolio with no targets
   df_viz %>%
     mutate(no_rep = completeness == 0) %>%
     count(no_rep, wt = mer_targets) %>%
     mutate(cum_share = n / sum(n))

   #completeness by partner
   df_viz %>%
     mutate(completeness = ifelse(completeness > 1, 1, completeness),
            filter_greys = if_else(!is.na(comp_prior_bin) & completeness <= 0.25 & mer_targets>= 1e5, "#CB181D", grey40k),
            facet_lab = paste0(toupper(partner_shortname), "\n",
                               ifelse(prtnr_comp > 1, "+100%",
                               percent(prtnr_comp,1)))
            ) %>%
     filter(!is.na(comp_prior_bin)) %>%
     ggplot() +
     annotate(geom = "rect",
              xmin = 1e5, ymin = -0.03,
              xmax = target_max + 1000000, ymax = 0.23,
              fill = grey30k, alpha = 0.25,
              color = grey50k, linetype = "dashed"
              ) +
     geom_point(aes(mer_targets, completeness, fill = filter_greys),
                size = 4, alpha = .75, shape = 21, stroke = 1, color = "white") +
     facet_wrap(~ fct_reorder(facet_lab, prtnr_comp, .desc = TRUE)) +
     scale_x_log10(label = comma) +
     scale_y_continuous(label = percent) +
     scale_fill_identity() +
     labs(x = "FY20 TX_CURR MER TARGETS", y = "HFR Site Reporting Completeness",
          subtitle = "HFR 2020.07 | TX_CURR",
          caption = "completeness capped at 100%; ordered by FY20 MER Targets
           HFR Data [2020-05-20]") +
     si_style() +
     theme(strip.text = element_text(face = "bold"),
           legend.title = element_text(family = "Source Sans Pro", color = "gray30"))

   #export
   h <- 10
   w <- (16/9) * h
   ggsave("out/CompletenessMechs07_sm.png", dpi = 330,
          height = h,
          width = w)

