# Wavelength 2.3.6
* Resolve bug in Q3 where hierarchy in DATIM was adjusted
* Update `iso_map` due to new hierarchy
* add "na" to `fix_noncompliance()`

# Wavelength 2.3.3
* Resolve bug that converted NAs to zeros when wide formats were used
* Restricts output value to integer (rounding any decimal values to the nearest integer)
* Fixes UIDs issues that may have hidden line break in cell
* Fix export of periods to be padded string, so 2020.10 does not appear as 2020.1

# Wavelength 2.3.2
* Add additional template option, Wide - Limited, for optional use during COVID Response
* Adjust validations and processing to account for additional template + options

# Wavelength 2.3.1
* Fix issue with ordering disconnect between wide/long with age and sex in `hfr_gather()`
* Replace any spaces in `indicator` with an underscore (`_`) in `hfr_munge_string()`
* Adjusted `pull_mer()` to handle FY20 HTS result modalities and temporary TX_CURR DATIM bug (FY20Q1 does not show up under `Oct 2019 to Sep 2020` only under quartelry results)
* Update `mypwd()` to work on unix OS, not just PC
* Add `hfr_filter_pd()` to filter when exporting processed files to restrict export to just the period of choice.

# Wavelength 2.3.0
* Add `hfr_restrict_cols()` to remove any non HFR columns submitted in template
* Adjust export to handle exporting by mechanims

# Wavelength 2.2.0
* Add validations to run inline with import
  * before import, check there are sheets to import (contain HFR in tab name), pull meta data in, and identify import/ignored tabs,   `validate_initial()`
  * after import, validate that the correct columns exists and if there are extras, `validate_import()`
  * at end, check columns, dates, orgunituids, mechanisms, indicators, and disaggs, `validate_output()`
* rename scripts for pulling DATIM data, `pull_()`

# Wavelength 2.1.0
* Added function to extract all OU/country hierarchies, `hierarchy_pull()`
* Added function to pull mechanism information from DATIM, `mech_pull()`
* Update `iso_map` data to be sourced from DATIM
* Centralize additional functions to a utlities script
* Moved `periodize_targets()` from `hfr_datim_api()` into own script and renamed `hfr_gap_target()`
* Added `curr_fy()` to return the current fiscal year
* Update `hfr_export()` to include period and handle all saving types
* Removed unused function, `identify_mechs()`
* Adjust `identify_ouuids()` to capture country UIDs under regional missions


# Wavelength 2.0.0
* Rename functions to conform to a model of `hfr_*()`
* Added function to read in HFR processed/output file, `hfr_read()`
* Update README

# Wavelength 1.0.0
* Update with period 2020.01 with standardized template
* Added function to import standardized template, `import_hfr()`
* Added function to adjust wide template to long, `gather_hfr()`
* Added function to handle adjustments to indicator names, age, sex, and mecahnism, `munge_string()`
* Added function to address issues with submissions, `fix_noncompliance()`
* Update `fix_date()` to handle different date formats submitted
* Added function to rectify non-HFR week days submitted, `round_date()`
* Update `assign_pds()` and `identify_pds()` to fix bug with fiscal year and be more automated
* Add function to aggregate data to reduce rows, `aggr_hfr()`
* Add function to formalize some of the data checks to the data, `run_checks()`

# Wavelength 0.0.0.9000

* Added function to import Lesotho data, `structure_lso()`
* Added function to run `structure_*()` based on ISO code, `unify_hfr()`
* Added function to arrange variables in select order for unified output, `order_vars()`
* Added function to import Burundi data, `structure_bdi()`
* Added function to pull out header row names, `extract_headers()`
* Added function to import Haiti dataset, `structure_hti()`
* Added function for reshaping long, `reshape_long()`
* Added function for importing Haiti that pulls in and combines multiple header rows, `identify_headers_hti()`
* Added function to import Uganda system output, `structure_tza()`
* Add function for exporting as a .txt, `export_hfd()`, including adding in an ISO code mapping table for naming `data(iso_map)`
* Added function to name operatingunits, `add_ou()` since this code was replicated in each dataset
* Added an indicator mapping for Uganda, `data(ind_map_uga)`, with full list of collected indicators and ones that we are currently collecting for OHA, breaking out disaggs.
* Added function to import Democratic Republic of the Congo partner files, `structure_cod()`
* Added age mapping for age bands, `data(age_map)` and `apply_agebands()`
* Added function to import Tanzania system output and weekly partner files, `structure_tza()`
* Added an indicator mapping for Tanznaia, `data(ind_map_tza)`, with full list of collected indicators and ones that we are currently collecting for OHA, breaking out disaggs.
* Added a `NEWS.md` file to track changes to the package.
