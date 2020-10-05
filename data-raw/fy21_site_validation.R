## PROJECT:  HFR
## AUTHOR:   A.CHAFETZ | USAID
## PURPOSE:  validation for FY21 reporting sites
## LICENSE:  MIT
## DATE:     2020-10-02
## UPDATED:  2020-10-05



# DEPENDENCIES ------------------------------------------------------------

library(tidyverse)
library(vroom)
library(glamr)
library(Wavelength)
library(openxlsx)
library(ICPIutilities)
library(glue)


# GLOBAL VARIABLES --------------------------------------------------------

  #DATIM data folder (HFR MER reference tables)
    fldr_datim <- "out/DATIM"

  #MSD
    fldr_msd <- "~/Data"

  #DATIM user name
    user <- "achafetz_global"

  #output folder
    fldr_out <- "out/siteval"
    dir.create(fldr_out, showWarnings = FALSE)

# IMPORT ------------------------------------------------------------------

  #import org hierarchy
    df_orgs <- return_latest(fldr_datim, "org") %>%
      vroom()

  #import USAID MER data
    df_mer <- list.files(fldr_datim, "FY20Q", full.names = TRUE) %>%
      map_dfr(vroom, col_types = c(.default = "c", fy = "i", mer_results = "d", mer_targets = "d"))

  #import USAID FY21 data
    df_msd <- list.files(fldr_msd, "OU_IM", full.names = TRUE) %>%
      read_rds()

  #import reporting level (community/facility)
    df_lvls <- get_outable(user, mypwd(user))


# MUNGE -------------------------------------------------------------------

  #limit MSD to FY21 USAID partners (back table for reference)
    df_msd <- df_msd %>%
      filter(fiscal_year == 2021,
             fundingagency == "USAID",
             indicator %in% c("HTS_TST", "TX_NEW", "TX_CURR", "PrEP_NEW", "VMMC_CIRC"),
             standardizeddisaggregate == "Total Numerator") %>%
      rename_official() %>%
      distinct(operatingunit, countryname, mech_code, mech_name, primepartner, indicator) %>%
      mutate(x = "X") %>%
      pivot_wider(names_from = indicator, names_prefix = "FY21\n", values_from = x) %>%
      mutate(operatingunit = ifelse(operatingunit!=countryname, glue("{operatingunit}/{countryname}"), operatingunit)) %>%
      select(-countryname)

  #table of reporting level for inner join (to drop higher hierarchy)
    df_lvls <- df_lvls %>%
      select(countryname, community, facility_lvl) %>%
      gather(type, level, -countryname) %>%
      mutate(type = str_remove(type, "_lvl"))

  #limit org hierarchy to just comm/fac
    df_orgs_cf <- df_orgs %>%
      inner_join(df_lvls) %>%
      relocate(type, .after = level) %>%
      select(-c(level, latitude, longitude))

  #agg to orgunit x mech x ind level (removing HTS_POS since captured by HTS_TST)
    df_mer_agg <- df_mer %>%
      rename_official() %>%
      filter(indicator != "HTS_TST_POS") %>%
      group_by(orgunituid, mech_code, mech_name, primepartner, indicator) %>%
      summarise(across(c(mer_results, mer_targets), sum, na.rm = TRUE)) %>%
      ungroup()

  #spread to have one obs by mech x orgunit
    df_mer_wide <- df_mer_agg %>%
      mutate(x = "KEEP") %>%
      select(-starts_with("mer")) %>%
      spread(indicator, x)

  #join USAID MER reporting/targets onto full org hierarchy
    df_full <- df_orgs_cf %>%
      left_join(df_mer_wide)

  #sort so orgunits with reporting are on top
    df_full <- df_full %>%
      arrange(operatingunit, countryname, desc(!is.na(mech_code)), orgunituid)

  #combine OU and country name & remove excess cols
    df_full <- df_full %>%
      mutate(operatingunit = ifelse(operatingunit!=countryname, glue("{operatingunit}/{countryname}"), operatingunit)) %>%
      select(-c(countryname, psnuuid, community, facility))

  #FY20 v FY21 mech ref table
   df_ref <- df_full %>%
     filter(!is.na(mech_code)) %>%
     rename_with(.cols = c(HTS_TST, PrEP_NEW, TX_CURR, TX_NEW, VMMC_CIRC), ~ paste0("FY20\n", .)) %>%
     mutate(across(starts_with("FY20"), ~ str_replace(. , "KEEP", "1") %>% as.integer)) %>%
     group_by(operatingunit, mech_code, mech_name, primepartner) %>%
     summarise(across(starts_with("FY20"), sum, na.rm = TRUE)) %>%
     ungroup() %>%
     mutate(across(starts_with("FY20"), ~ ifelse(. == 0, NA_character_, "X"))) %>%
     full_join(df_msd) %>%
     arrange(operatingunit, mech_code)

  #mech status to flag closing mechs
   df_status <- df_ref %>%
     select(mech_code, starts_with("FY")) %>%
     pivot_longer(starts_with("FY"),
                  names_to = c("fy", "indicator"), names_sep = "\\n",
                  values_to = "x") %>%
     filter(!is.na(x)) %>%
     distinct(mech_code, fy) %>%
     mutate(x = 1) %>%
     spread(fy, x) %>%
     mutate(status = ifelse(is.na(FY21), "ending", "current")) %>%
     select(-starts_with("FY"))

  #combine mech info
    df_full <- df_full %>%
      left_join(df_status) %>%
      mutate(mech_code = ifelse(status == "ending", glue("(!) {mech_code}"), mech_code),
             mech_partner = glue("{mech_code}: {mech_name} [{primepartner}]"),
             mech_partner = ifelse(mech_partner == "NA: NA [NA]", NA_character_, mech_partner)) %>%
      relocate(mech_partner, .before = mech_code) %>%
      select(-c(mech_code, mech_name, primepartner))
    df_ref <- df_ref %>%
      left_join(df_status) %>%
      mutate(mech_code = ifelse(status == "ending", glue("(!) {mech_code}"), mech_code),
             mech_partner = glue("{mech_code}: {mech_name} [{primepartner}]"),
             mech_partner = ifelse(mech_partner == "NA: NA [NA]", NA_character_, mech_partner)) %>%
      relocate(mech_partner, .before = mech_code) %>%
      select(-c(mech_code, mech_name, primepartner)) %>%
      relocate(status, .after = mech_partner)

  #change validation for ending mechanisms
    df_full <- df_full %>%
      mutate(across(c(HTS_TST:VMMC_CIRC), ~ ifelse(status == "ending" & . == "KEEP", "DROP", .))) %>%
      select(-status)

  #add validation options
    df_validation <- tibble(type = c("KEEP", "DROP", "ADD"))


  #create instructions tab
    df_inst <- c("Instructions\n",
                 "Without having site targets set in COP20, we don't have a list of sites that should be reporting HFR data. As a result, we cannot assess the representativeness and therefore value of the data. We need your help in validating the sites your partners will be working in this fiscal year. Please return this workbook to HFRG by October 31, 2020.\n",
                 "We have designed this workbook to help this process. The main tab, Site List, is where all the work is occurring. The far left column consists of all the organization units (facilities or communities, which are identified in column C) that had results and/or targets in FY20. Facilities or communities that are bold are the active sites for FY21, those with a mechanism continuing into the year reporting on one of the HFR indicators. \n",
                 "The mechanism/partner info in Column G provides the mechanism information for the USAID partners working in the facility or community in FY20. The full facility/community list is included, but only USAID partners have mechanism information and are at the top of the list. If a mechanism is not continuing into FY21, i.e. it has no targets, the mechanism information in this column starts with \"(!)\" and is in brown. If a partner is replacing them, you can use the drop down to choose the partner. This action can be performed by selecting the cell in column G and then hitting the down arrow that appears to the right of the cell.\n",
                 "The columns to the far right - HTS_TST-VMMC_CIRC (columns H - L) capture whether there were any results/targets reported in FY20. If the mechanism is continuing into FY21, the cells will show \"KEEP\" indicating the mechanism will continue to work in these areas. For mechanisms that are ending, their cells will read \"DROP\".  If a new USAID mechanism will pick up the work in the facility/community in FY21, you will need to identify the new mechanism (via the dropdown list provided) and change the indicator drop downs to \"ADD\". \n",
                 "If a mechanism will be working in a new facility/community that USAID was not in before, you can find the full list below the USAID ones. You will need to select the partner from the drop down in Column G and  then note which indicators (Columns H-L) will be reported by selecting \"ADD\" to those indicator columns.\n",
                 "If an additional partner will be working in a facility/community, you can insert a new row (right click on a cell > Insert > Table Rows Above) and the copy the facility/community information from Columns A - F. You will need to select the partner from the drop down in Column G and then note which indicators (Columns H-L) will be reported by selecting \"ADD\" to those indicator columns.\n",
                 "The last tab in the workbook, Mech Ref List, just provides you a list of which mechanisms in FY20 and FY21. If a mechanism has no targets (for HFR indicators) in FY21 its status in Column C will say \"ending. This tab is just for reference and doesn't not require any updating or editing. \n",
                 "If you have any questions, please reach out to HFRG, oha_hfr@usaid.gov. Site validations are due by October 30, 2020. WE WILL BE UNABLE TO PROCESS ANY OF YOUR FY21 HFR SUBMISSIONS IF WE DO NOT RECEIVE THIS LIST FROM YOU. ")  %>%
      as_tibble() %>%
      rename(`FY21 HFR SITE VALIDATION` = value)

# EXPORT ------------------------------------------------------------------

  setup_wkbk <- function(ou){

    #filter dataframes to select OU
      df_full_ou <- filter(df_full, operatingunit == ou)
      df_ref_ou <- filter(df_ref, operatingunit == ou)

    #create workbook
      wb <- createWorkbook()

    #update font
      modifyBaseFont(wb, fontName = "Calibri Light")

    #add instructions tab
      addWorksheet(wb = wb, sheetName = "Instructions", gridLines = FALSE)
      writeData(wb, sheet = "Instructions", df_inst, startCol = 2)

    #format text
      nrow_inst <- nrow(df_inst) + 1
      setColWidths(wb, sheet = "Instructions", cols = 1, widths = 1.5)
      setColWidths(wb, sheet = "Instructions", cols = 2, widths = 110)
      style_inst_hdr <- createStyle(fontName = "Calibri", fontSize = 16, textDecoration = "bold")
      addStyle(wb, sheet = "Instructions", style = style_inst_hdr, rows = 1, cols = 2)
      style_inst <- createStyle(wrapText = TRUE, valign = "top")
      addStyle(wb, sheet = "Instructions", style = style_inst, rows = 1:nrow_inst, cols = 2)

    #add Site list worksheet as a table
      addWorksheet(wb = wb, sheetName = "Site List", gridLines = FALSE, zoom = 90)
      writeDataTable(wb, sheet = "Site List", df_full_ou, tableStyle = "TableStyleLight8", withFilter = TRUE)

    #add a copy of the original pre editing
      addWorksheet(wb = wb, sheetName = "Orig Site List", visible = FALSE)
      writeData(wb, sheet = "Orig Site List", df_full_ou, withFilter = TRUE)
      sheetVisibility(wb)[3] <- "veryHidden" # hide sheet from UI

    #freeze top row
      freezePane(wb, sheet = "Site List", firstRow = TRUE, firstCol = TRUE)

    #add ind reporting validations
      col_start <- which(colnames(df_full_ou)=="HTS_TST")
      col_end <- which(colnames(df_full_ou)=="VMMC_CIRC")
      nrow <- nrow(df_full_ou) + 1
      addWorksheet(wb, sheetName = "rs", visible = FALSE)
      writeData(wb, sheet = "rs", df_validation)
      nrow_valid <- nrow(df_validation)+1
      createNamedRegion(wb, sheet = "rs", name = "type", cols = 1, rows = 2:nrow_valid)
      dataValidation(wb, sheet = "Site List",
                     cols = col_start:col_end,
                     rows = 2:nrow,
                     type = "list",
                     value= "type")

    #increase column sizes
      setColWidths(wb, sheet = "Site List", cols = which(colnames(df_full_ou)=="orgunit"), widths = 42.14)
      setColWidths(wb, sheet = "Site List", cols = which(colnames(df_full_ou)=="orgunituid"), widths = 15.57)
      # setColWidths(wb, sheet = "Site List", cols = which(colnames(df_full_ou)=="orgunituid"), hidden = TRUE)
      setColWidths(wb, sheet = "Site List", cols = which(colnames(df_full_ou)=="mech_partner"), widths = 75)
      setColWidths(wb, sheet = "Site List", cols = col_start:col_end, widths = 13.71) #indicators

    #header styles
      style_hdr <- createStyle(fontName = "Calibri", textDecoration = "bold")
      addStyle(wb, sheet = "Site List", style = style_hdr, rows = 1, cols = 1:length(df_full_ou))
      style_hdr2 <- createStyle(fontName = "Calibri", textDecoration = "bold", halign = "center")
      addStyle(wb, sheet = "Site List", style = style_hdr, rows = 1, cols = col_start:col_end)

    #style ind valdiation
      style_vld <- createStyle(fontName = "Calibri", halign = "center", textDecoration = "bold")
      addStyle(wb, sheet = "Site List", style = style_vld, rows = 2:nrow, cols = col_start:col_end, gridExpand = TRUE)

    #add the reference table
      addWorksheet(wb, sheetName = "Mech Ref List")
      writeData(wb, sheet = "Mech Ref List", df_ref_ou)

    #adjust 1st row height
      setRowHeights(wb, sheet = "Mech Ref List", rows = 1, heights = 40)

    #header styles
      ref_style_hdr <- createStyle(fontName = "Calibri", textDecoration = "bold", wrapText = TRUE)
      addStyle(wb, sheet = "Mech Ref List", style = ref_style_hdr, rows = 1, cols = 1:length(df_ref_ou))

    #style ind
      ref_col_start <- which(colnames(df_ref_ou)=="FY20\nHTS_TST")
      ref_col_end <- which(colnames(df_ref_ou)=="FY21\nVMMC_CIRC")
      ref_nrow <- nrow(df_ref_ou) + 1
      ref_style_hdr_ind <- createStyle(fontName = "Calibri", textDecoration = "bold", halign = "center", wrapText = TRUE)
      ref_style_ind <- createStyle(halign = "center")
      addStyle(wb, sheet = "Mech Ref List", style = ref_style_hdr_ind, rows = 1, cols = ref_col_start:ref_col_end)
      addStyle(wb, sheet = "Mech Ref List", style = ref_style_ind, rows = 2:ref_nrow, cols = ref_col_start:ref_col_end, gridExpand = TRUE)

    #increase column sizes
      setColWidths(wb, sheet = "Mech Ref List", cols = which(colnames(df_ref_ou)=="operatingunit"), widths = 17)
      setColWidths(wb, sheet = "Mech Ref List", cols = which(colnames(df_ref_ou)=="mech_partner"), widths = 75)

    #add mech_partner dropdown
      nrow_mech <- nrow(df_ref_ou)+1
      createNamedRegion(wb, sheet = "Mech Ref List", name = "mechlist",
                        cols =  which(colnames(df_ref_ou)=="mech_partner"),
                        rows = 2:nrow_mech)
      dataValidation(wb, sheet = "Site List",
                     cols = which(colnames(df_full_ou)=="mech_partner"),
                     rows = 2:nrow,
                     type = "list",
                     value= "mechlist")

    #flag ending mechanisms conditional formatting
      style_oldmech <- createStyle(fontColour = "#a6611a")
      conditionalFormatting(wb, sheet = "Site List",
                            cols = which(colnames(df_full_ou)=="mech_partner"),
                            rows = 2:nrow,
                            rule = "(!)",
                            type = "contains",
                            style = style_oldmech
                            )

    #highlight ind choices
      style_type_keep <- createStyle(fontColour = "#FFFFFF", bgFill = "#8C8985", border = c("left", "right"), borderColour = "#FFFFFF")
      conditionalFormatting(wb, sheet = "Site List",
                            cols = col_start:col_end,
                            rows = 2:nrow,
                            rule = "KEEP",
                            type = "contains",
                            style = style_type_keep)

      style_type_drop <- createStyle(fontColour = "#FFFFFF", bgFill = "#a6611a", border = c("left", "right"), borderColour = "#FFFFFF")
      conditionalFormatting(wb, sheet = "Site List",
                            cols = col_start:col_end,
                            rows = 2:nrow,
                            rule = "DROP",
                            type = "contains",
                            style = style_type_drop)

      style_type_add <- createStyle(fontColour = "#FFFFFF", bgFill = "#5ab4ac", border = c("left", "right"), borderColour = "#FFFFFF")
      conditionalFormatting(wb, sheet = "Site List",
                            cols = col_start:col_end,
                            rows = 2:nrow,
                            rule = "ADD",
                            type = "contains",
                            style = style_type_add)

    #bold if active site
      style_active <- createStyle(fontName = "Calibri", textDecoration = "bold")
      conditionalFormatting(wb, sheet = "Site List",
                            cols = 1,
                            rows = 2:nrow,
                            rule = '=AND(LEN($G2)>0,NOT(ISNUMBER(SEARCH("(!)", $G2))))',
                            type = "expression",
                            style = style_active)

    #clean up name for saving
      ou_clean <- ou %>%
        str_replace("West Africa Region", "WAR") %>%
        str_replace("Western Hemisphere Region", "WHR") %>%
        str_replace("Democratic Republic of the Congo", "DRC") %>%
        str_replace("/", "-") %>%
        str_remove_all(" |'")

    #save
      saveWorkbook(wb, file.path(fldr_out, glue("HFR_FY21_SiteValidation_{ou_clean}.xlsx")), overwrite = TRUE)

  }



    ou_list <- unique(df_full$operatingunit)

    walk(ou_list, setup_wkbk)
