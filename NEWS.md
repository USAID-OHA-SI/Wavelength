# Wavelength 2.0.0
* Rename functions to conform to a model of `hfr_*()`

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
