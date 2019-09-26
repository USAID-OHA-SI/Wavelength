# Wavelength

[![Travis build status](https://travis-ci.org/USAID-OHA-SI/Wavelength.svg?branch=master)](https://travis-ci.org/USAID-OHA-SI/Wavelength)

### OHA High Frequency Reporting Munging

![workflow](https://user-images.githubusercontent.com/8933069/56661405-abb9e300-666f-11e9-8a25-eb9e9c370c4a.png)


This project is aimed at collecting and standardizing various High Frequency Reporting (HFR) data, i.e. monthly collection/submission of weekly data, from USAID PEPFAR operating units and partners. A standardized output will make it easier to conduct analysis at the HQ level across all the operating units USAID supports. The [guidance](https://docs.google.com/document/d/1-j4NP0iQBMuBs5Dpny-b3ax7GRo3144F8tM1OBu3Y38/edit?usp=sharing) as well as futher documentation and data can be found on [Google Drive](https://drive.google.com/open?id=14lcqRwZaR7ZhyhF2-NoAwuhF6p4dhVzc).

Notes about each of the countries can be found in the [wiki](https://github.com/USAID-OHA-SI/Wavelength/wiki).

Available features and changes are noted in [NEWS.md](https://github.com/USAID-OHA-SI/Wavelength/blob/master/NEWS.md)

## Workflow

The HFR process works on a four week cycle, called a period, detailed in the [HFR calendar](https://drive.google.com/file/d/13fNe1-5sw8VTiBHD7n_t0NyCvKWNKvlP/view?usp=sharing). At the end of each period, USAID implementing partners submit data via [submission templates](https://drive.google.com/open?id=1k0KXIfSwyROCV4ULiZN3qhw6qYH3ekNA) on key indicators data disaggrated by week to the in country USAID team who in turn send it to Washington. 

A team in USAID Washington on the OHA/SIEI division, process the submission to ensure they follow the guidance and have no major errors. This processing occurs either via [Tableau Prep](https://drive.google.com/open?id=1s1rtVaJhPpTOyb_E_sPVBghiKEVE0fLM) or in R via this Wavelength package. 

After the data is standardized into tidy format, PEPFAR MER results and targets from the latest quarter of the current year are added to the global HFR dataset. This new dataset is then migrated into a Tableau workbook that provides a detailed look at trends over time and how weekly results compare the remaining weekly average targets needed to achieve the fiscal year targets.
 
## Creating a flat file output for one or more Operating Units

You can create a flat file by installing this package, `Wavelength` and running `unify_hfr()`. You will need to know the country ISO code, which you can [look up here](https://github.com/USAID-OHA-SI/Wavelength/blob/master/data-raw/ISOcodes_PEPFAR_Countries.csv). If you need to combine multiple partner files, you will need to utitlize the `map_dfr()` function from the `purrr` package. 

Below is an example code to use to create a flat file for a country. In addition to the country ISO code, you will need the country/partner data files and to specify the output folder path where you want the file to be saved.

NOTE: This currently only works for a handful of countries as most the processing to date occurs in Tableau Prep. Given the previous flexiblity around submissions, the templates may change each period and the scripts may need to be updated to account for the structural cahgnes 


```{r}
## SETUP

  #install
    install.packages("devtools")
    devtools::install_github("USAID-OHA-SI/Wavelength")
    
  #load the package
    library(Wavelength)

## EXAMPLE WITH ONE FILE

  #file path for the 
    path <- "~/WeeklyData/Uganda"
    output_folder <- "~/WeeklyData/Output"
  #process Excel file for Uganda, ISO code = UGA
    unify_hfr(ou_iso = "UGA", 
              filepath = path, 
              folderpath_output = output_folder)
            
## EXAMPLE WITH MULTIPLE FILES

  #install purrr if you don't already have it
    install.packages("purrr")
  #file path for the 
    files <- list.file("~/WeeklyData/Tanzania", full.names = TRUE)
    output_folder <- "~/WeeklyData/Output"
  #process Excel file for Tanzania, ISO code = TZA
    purrr::map_dfr(.x = files,
                   .f = ~ unify_hfr(ou_iso = "TZA", 
                                    filepath = .x, 
                                    folderpath_output = output_folder))
```

## Pulling DATIM data 

The official PEPFAR MER data are stored in a DHIS2 system called DATIM. Indicators are collected on a quarterly basis and are compared to targets for each fiscal year. We use the MER data to create gap targets (ie how much of the fiscal year target needs to be completed with the cumulative quarterly results so far) as well as to identify offical partner names as well as the offical organizational hierarchy. 

DATIM allows for API call so as to access site level results and targets. The `extract_datim()` script allows users with DATIM account to extract this data, which is also stored on the HFR Google Drive.

To run `extract_datim()` you will need to install additional packages (`keyringr`, `curl`, `httr`, and `jsonlite`). Since DATIM needs your credentials, you will need to securely store them on your machine, following the documentation laided out in the [`keyringr` package vignette](https://cran.r-project.org/web/packages/keyringr/vignettes/Avoiding_plain_text_passwords_in_R_with_keyringr.html).

```{r}
#ACCESSING AND STORING DATIM DATA

  #identify the operating units you want to pull targets for
    ou_list <- c("Kenya", "Ethiopia")
  
  #provide your credentials
    myuser <- ""
    
  #API Pull
    purrr::walk(.x = ou_list,
                .f = ~ extract_datim(
                                     ou_name = .x,
                                     username = myuser, 
                                     password = mypwd(myuser), # or if not stored, just your password
                                     quarters_complete = 3, #current quarter of data availability
                                     folderpath_output = "out/DATIM" #path on your machine
                                     )
               )
```

## Creating a global HFR (and MER) dataset

The global HFR dataset that feeds into the Tableau workbook pulls from all the processed country HFR (all historic data to date) and the locally stored DATIM data. The `append_sources()` function brings together these data sources for the Tableau workbook. You willneed to have the HFR data stored in one folder and DATIM data stored in another. If you have countries with historic data but not for the current period, you should include those files in the HFR folder so they can be included in the global dataset.


```{r}
#APPEND DATA

  #create an appended data source
    df_combo <- append_sources(folderpath_hfr  = "out/processed",
                               folderpath_datim = "out/DATIM",
                               start_date = "2019-06-03",
                               weeks = 13, #update with current HFR dates
                               max_date = TRUE,
                               folderpath_output = "out/joint")
```

Checks can can should be run on the global dataset, ensuring that everything was uniformly processed. Variable names and their contents should be compared against the [HFR codebook](https://github.com/USAID-OHA-SI/Wavelength/wiki/HFR-Codebook). Some of the checks can be found in the `data-raw/scrapwork.R` script. Items to check include:
  - all variables are lower case and match the codebook
  - ensuring dates all match the HFR reporting calendar
  - indicators are only HFR indicators and spelling match
  - disaggregate components (`agecoarse`, `sex`, and `otherdisaggregate`) are only what is noted in the codebook


## Populating the HFR Tableau workbook 

With the global dataset created, the [Tableau workbook](https://drive.google.com/drive/u/0/folders/1-V28fJK5XMu_DcaZXNf8Ot_441dADESG) can now be updated with the new data. At this point, country specific files are created and then distributed to country teams.


---

*Disclaimer: The findings, interpretation, and conclusions expressed herein are those of the authors and do not necessarily reflect the views of United States Agency for International Development. All errors remain our own.*
