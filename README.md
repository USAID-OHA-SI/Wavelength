# Wavelength

[![Travis build status](https://travis-ci.org/USAID-OHA-SI/Wavelength.svg?branch=master)](https://travis-ci.org/USAID-OHA-SI/Wavelength)

### OHA High Frequency Reporting Munging

![workflow](https://user-images.githubusercontent.com/8933069/56661405-abb9e300-666f-11e9-8a25-eb9e9c370c4a.png)


This project is aimed at collecting and standardizing various High Frequency Reporting (HFR), eg weekly/monthly, files from USAID operating units and partners. A standardized output will make it easier to conduct analysis at the HQ level across all the operating units USAID supports. The [work plan](https://docs.google.com/document/d/10kLZofiJE4ueDOoOIJ49SIdT6iFK3VVm9J3_DdSpW0A/edit?usp=sharing) as well as futher documentation and data can be found on [Google Drive](https://drive.google.com/open?id=14lcqRwZaR7ZhyhF2-NoAwuhF6p4dhVzc).

Notes about each of the countries can be found in the [wiki](https://github.com/USAID-OHA-SI/Wavelength/wiki).

Available features and changes are noted in [NEWS.md](https://github.com/USAID-OHA-SI/Wavelength/blob/master/NEWS.md)

## Creating a flat file output

You can create a flat file by installing this package, `Wavelength` and running `unify_hfr()`. You will need to know the country ISO code, which you can [look up here](https://github.com/USAID-OHA-SI/Wavelength/blob/master/data-raw/ISOcodes_PEPFAR_Countries.csv). If you need to combine multiple partner files, you will need to utitlize the `map_dfr()` function from the `purrr` package. 

Below is an example code to use to create a flat file for a country. In addition to the country ISO code, you will need the country/partner data files and to specify the output folder path where you want the file to be saved.

NOTE: This currently only works for a handful of countries who have already submitted their HFR data. As of April 30, the countries include: Burundi, South Africa, Tanzania, and Uganda.


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



---

*Disclaimer: The findings, interpretation, and conclusions expressed herein are those of the authors and do not necessarily reflect the views of United States Agency for International Development. All errors remain our own.*
