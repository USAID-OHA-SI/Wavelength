 <!-- badges: start -->
  [![R-CMD-check](https://github.com/USAID-OHA-SI/Wavelength/workflows/R-CMD-check/badge.svg)](https://github.com/USAID-OHA-SI/Wavelength/actions)
  <!-- badges: end -->

<img src='man/figures/logo.png' align="right" height="120" />

# Wavelength

### OHA High Frequency Reporting Munging

![HFR_Cycle](https://user-images.githubusercontent.com/8933069/70257042-92c1f600-1757-11ea-852a-69a6cb6beb61.png)

This project is aimed at collecting and standardizing various High Frequency Reporting (HFR) data, i.e. monthly collection/submission of weekly data, from USAID PEPFAR operating units and partners. A standardized output will make it easier to conduct analysis at the HQ level across all the operating units USAID supports. As of period 2020.01, all submissions are required to be sent in either one of [two templates](https://drive.google.com/open?id=1k0KXIfSwyROCV4ULiZN3qhw6qYH3ekNA) in order to standardize the process. The [guidance](https://docs.google.com/document/d/1-j4NP0iQBMuBs5Dpny-b3ax7GRo3144F8tM1OBu3Y38/edit?usp=sharing) as well as futher documentation and data can be found on [Google Drive](https://drive.google.com/open?id=14lcqRwZaR7ZhyhF2-NoAwuhF6p4dhVzc).

Available features and changes are noted in [NEWS.md](https://github.com/USAID-OHA-SI/Wavelength/blob/master/NEWS.md)

## Workflow

![HFR_Process](https://user-images.githubusercontent.com/8933069/70255403-b9caf880-1754-11ea-8e0a-b47e4edb1279.png)

The HFR process works on a four week cycle, called a period, detailed in the [HFR calendar](https://drive.google.com/file/d/13fNe1-5sw8VTiBHD7n_t0NyCvKWNKvlP/view?usp=sharing). At the end of each period, USAID implementing partners submit data via [submission templates](https://drive.google.com/open?id=1k0KXIfSwyROCV4ULiZN3qhw6qYH3ekNA) on key indicators data disaggrated by week to the in country USAID team who in turn send it to Washington. 

A team in USAID Washington on the OHA/SIEI division, process and validate the submission to ensure they follow the guidance and have no major errors. This processing occurs in R via this Wavelength package. The processed file is imported into a relational database that contains pertinent PEPFAR information, including PEPFAR MER results and targets from the latest quarter of the current year are added to the global HFR dataset

A [Tableau workbook](https://tableau.usaid.gov/#/views/HFRDashboard/Intro?:iid=1) located on USAID's Tableau server is linked to SQL View extract from the database. The standard visuals provide a detailed look at trends over time and how weekly results compare the remaining weekly average targets needed to achieve the fiscal year targets.
 
## Creating a flat file output for one or more Operating Units

You can create a flat file by installing this package, `Wavelength` and running `hfr_process_template()`. If you need to combine multiple country/partner files, you will need to utitlize the `map_dfr()` function from the `purrr` package. 

Below is an example code to use to create a flat file for a country submission. You will need the country/partner data files and to specify the output folder path where you want the file to be saved.


```{r}
## SETUP

  #install
    install.packages("devtools")
    devtools::install_github("USAID-OHA-SI/Wavelength")
    
  #load the package
    library(Wavelength)

## EXAMPLE WITH ONE FILE

  #file path for the 
    path <- "~/WeeklyData/Saturn"
    output_folder <- "~/WeeklyData/Output"
  #process Excel file for Saturn
    hfr_process_template(path, output_folder)
  #process Excel file for Saturn with full validation
    datim_folder <- "~/Datim"
    hfr_process_template(path, output_folder, datim_path = datim_folder)
            
## EXAMPLE WITH MULTIPLE FILES

  #install purrr if you don't already have it
    install.packages("purrr")
  #file path for the 
    files <- list.file("~/WeeklyData", full.names = TRUE)
    output_folder <- "~/WeeklyData/Output"
  #process Excel file for all submitted file in WeeklyData
    purrr::map_dfr(.x = files,
                   .f = ~ hfr_process_template(.x, output_folder)
  #process Excel file for all submitted file in WeeklyData with full validation
    datim_folder <- "~/Datim"
    purrr::map_dfr(.x = files,
                   .f = ~ hfr_process_template(.x, output_folder, datim_path = datim_folder)
```
## Reading in HFR data locally

If you have an HFR dataset stored locally and want to read it into R, you can use the `hfr_read()` function. This function utiltizes `vroom` for quicker import times and makes adjustments for the various column types without R having to guess.

```{r}
## EXAMPLE READING IN A PROCESSED HFR FILE

  #HFR file path
    path <- "~/WeeklyData/Output/HFR_2020.01_Mars_20201123.csv"
  #process Excel file for Saturn
    df_hfr_mars <- hfr_read(path)
```

## Pulling DATIM data 

The official PEPFAR MER data are stored in a DHIS2 system called DATIM. Indicators are collected on a quarterly basis and are compared to targets for each fiscal year. We use the MER data to create gap targets (ie how much of the fiscal year target needs to be completed with the cumulative quarterly results so far) as well as to identify offical partner names as well as the offical organizational hierarchy. 

DATIM allows for API call so as to access site level results and targets. The `pull_mer()` script allows users with DATIM account to extract this data, which is also stored on the HFR Google Drive. In addition to `pull_mer()` two other API call extract org unit information from DATIM (`pull_hierarchy()`) and mechanism and partner information (`pull_mech()`).  

To run `pull_mer()` you will need to install additional packages (`keyringr`, `curl`, `httr`, and `jsonlite`). Since DATIM needs your credentials, you will need to securely store them on your machine, following the documentation laided out in the [`keyringr` package vignette](https://cran.r-project.org/web/packages/keyringr/vignettes/Avoiding_plain_text_passwords_in_R_with_keyringr.html).

```{r}
#ACCESSING AND STORING DATIM DATA

  #identify the operating units you want to pull targets for
    ou_list <- c("Saturn", "Mars")
  
  #provide your credentials
    myuser <- ""
    
  #API MER Pull
    purrr::walk(.x = ou_list,
                .f = ~ pull_mer(
                                ou_name = .x,
                                username = myuser, 
                                password = mypwd(myuser), # or if not stored, just your password in quotes
                                quarters_complete = 3, #current quarter of data availability
                                folderpath_output = "out/DATIM" #path on your machine
                                )
               )
               
  #pull hierarchy
    ouuids <- identify_ouuids(myuser, mypwd(myuser)) %>% dplyr::pull(id)
    df_orgs <- purrr::map_dfr(.x = ouuids,
                              .f = ~ pull_hierarchy(.x, myuser, mypwd(myuser)))
    
  #pull mechanism info
    df_mech_info <- pull_mech()
```

## Creating a global HFR (and MER) dataset

The global HFR dataset that feeds into the Tableau workbook on the server pulls from all the processed country HFR (all historic data to date) and the locally stored DATIM data. This process is now conducted in the database, but previously it was run locally. The `hfr_append_sources()` function brings together these data sources for the Tableau workbook. You will need to have the HFR data stored in one folder and DATIM data stored in another. If you have countries with historic data but not for the current period, you should include those files in the HFR folder so they can be included in the global dataset.


```{r}
#APPEND DATA

  #create an appended data source
    df_combo <- hfr_append_sources(folderpath_hfr  = "out/processed",
                                   folderpath_datim = "out/DATIM",
                                   start_date = "2019-06-03",
                                   weeks = 4, #update with current HFR dates
                                   max_date = TRUE)
```

## Validations
Validation checks are run when processing the submitted data, ensuring that everything is there and uniformly processed. Some of the checks include:
  - all variables are lower case and match the codebook
  - ensuring dates all match the HFR reporting calendar
  - indicators are only HFR indicators and spelling match
  - disaggregate components (`agecoarse`, `sex`, and `otherdisaggregate`) are only what is noted in the codebook
These validations are built into the `hfr_process_template()` at various points via the `validation_*()` scripts and appear inline when processing.
```{r}
#VALIDATE DATA
  #submission file path
    path <- "~/WeeklyData/Saturn"
  #validations appear in the console
    hfr_process_template(path)
  #alternative, validate, adjusting the submission date automatically
    hfr_process_template(path, round_hfrdate = TRUE)
```

## Extra validations
Extra validation checks can be run after processing the submitted data, and correction / update made right before data ingestion :
  - all operating units are valid
  - all orgunit uids are valid and matched with their respective operating unit
  - all mechanism codes are valid and matched with their respective operating unit
  - age coarse and sex are valid options
  - val is either NA or a positive integer
These extra validations functions are in `hfr_qda_utilities` and `hfr_dqa_checks` files.

Set hfr data folder and load look up data 

```{r} 
  # HFR Data
  dir_data <- "..."
  
  # HFR Period dates
    valid_dates <- Wavelength::hfr_identify_pds(fy=2020)

  # ORG Levels
    org_levels <- read_csv(file = "file_orglevels")

  # ORGs
    orgs <- read_csv(file = "file_orgs")

  # Mechanisms
    ims <- read_csv(file = "file_mechanisms")
```

Validate and report errors for HFR forenames: this will list out all errors related to ISO3 codes and Mechanism Code

```{r}
  # Validate processed files
    hfr_processed_files <- validate_submissions(levels = org_levels,
                                            ims = ims,
                                            pfolder = dir_hfr_pd208,
                                            pattern = "HFR_2020.08")

  # Report Errors from processed files
    report_submissions_errors(hfr_processed_files, 
                              mechanisms = ims, 
                              export = F)
```

Validate processed file content (focus on operating units)

```{r}
  # Read data from all files
    hfr_data <- hfr_read_all(dir_data, 
                             pattern = 'HFR_2020.xx', 
                             source = T)

  # Check for invalid operating units
    hfr_data %>%
        is_ou_valid(df_orgs = orgs) %>%
        filter(valid_ou == FALSE) %>%
        distinct(source, operatingunit, valid_ou) %>%
        arrange(source)
```

Fix operating units with errors (using mech code and/or country iso3 code)

```{r}
  # Use mechanism code to update operating units
    hfr_data <- hfr_data %>%
        update_operatingunits(levels=org_levels, orgs=orgs, ims=ims)
        
  # Check if there are still any invalid operating units (for invalid mech_code)
    hfr_data %>%
        is_ou_valid(df_orgs = orgs) %>%
        filter(valid_ou == FALSE) %>%
        View()

  # Use iso3 code (second attempt)
    hfr_data <- hfr_data %>%
        update_operatingunits(levels=org_levels, orgs=orgs)
  
  # Check again for invalid operating units 
    hfr_data %>%
        is_ou_valid(df_orgs = orgs) %>%
        filter(valid_ou == FALSE) %>%
        View()
```

Run a full validation on the data (with all the operating units errors fixed)

```{r}
  # validate hfr data from processed files and return errors
    errors <- validate_hfr_data(hfr_data, org_levels, orgs, ims, valid_dates)
  
  # Explore the errors
    errors %>% glimpse()
```

Confirm all the validations by marking valid processed files as validated files

```{r}
  # update the name of files
    confirm_validations(hfr_data = hfr_data, 
                        hfr_errors = errors, 
                        dir_files = dir_data)
```

Note: data ingesting can now proceed with only validated files while the rest is being fixed / updated. 


---

*Disclaimer: The findings, interpretation, and conclusions expressed herein are those of the authors and do not necessarily reflect the views of United States Agency for International Development. All errors remain our own.*
