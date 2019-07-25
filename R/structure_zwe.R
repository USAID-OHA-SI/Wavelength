
filepath <- "C:/Users/GHFP/Documents/data/HFR/zwe/Zimbabwe HFR Workbook 23 July 2019.xlsx"

# import and structure Zimbabwe

# import

    sheet <- filepath %>%
      readxl::excel_sheets(.) %>%
      stringr::str_subset(pattern = "Weeks_June10_July1")
    
    df <- readxl::read_excel(filepath, sheet)
    
#clean var names and values
    
    df <- df %>% 
      dplyr::rename(partner = `Partner name`,
                    operatingunit = `Operating Unit`,
                    reporting_freq = Reporting_Frequency,
                    mechanismid = Mechanism_ID,
                    facility = SiteName,
                    indicator = Indicator,
                    agecoarse = Agecoarse,
                    sex = Sex,
                    value = Value,
                    date = Reporting_Week_Starting) %>% 
    dplyr::filter(value > 0,
                  agecoarse != "All") %>% 
    tibble::add_column(disaggregate = "",
                       fundingagency = "USAID") %>% 
    dplyr::mutate(fy = lubridate::year(date))
    
#tidy indicator 
    
    df <- df %>% 
      dplyr::filter(indicator %in% c("HTS_TST", "HTS_TST_POS", "PrEP_NEW", "TX_NEW", "TX_CURR", "VMMC_CIRC"))
    
# add columns
    df <- df %>% 
    dplyr::mutate(disaggregate = dplyr::case_when(indicator %in% c("TX_CURR", "TX_NEW") ~ "Age/Sex"),
                  disaggregate = dplyr::case_when(indicator %in% c("HTS_TST_POS", "HTS_TST") ~ "Age/Sex/Resultstatus"))
       
    
    