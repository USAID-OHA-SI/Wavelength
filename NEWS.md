# Wavelength 2.2.0
* Add validations to run inline with import
  * before import, check there are sheets to import (contain HFR in tab name), pull meta data in, and identify import/ignored tabs,   `validate_initial()`
  * after import, validate that the correct columns exists and if there are extras, `validate_import()`
  * at end, check columns, dates, orgunituids, mechanisms, indicators, and disaggs, `validate_output()`
*rename scripts for pulling DATIM data, `pull_()`

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
