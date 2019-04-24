# Wavelength 0.0.0.9000

* Added function to pull out header row names, `extract_headers()`
* Added function to import Hait dataset, `structure_hti()`
* Added function for reshaping long, `reshape_long()`
* Added function for importing Haiti that pulls in and combines multiple header rows, `identify_headers_hti()`
* Added function to import Uganda system output, `structure_tza()`
* Add function for exporting as a .txt, `export_hfd()`, including adding in an ISO code mapping table for naming `data(iso_map)`
* Added function to name operatingunits, `add_ou()` since this code was replicated in each dataset
* Added an indicator mapping for Uganda, `data(ind_map_uga)`, with full list of collected indicators and ones that we are currently collecting for OHA, breaking out disaggs.
* Added function to import Democratic Republic of the Congo partner files, `structure_cod()`
* Added age mapping for age bands, `data(age_map)` and `apply_agebands()`
* Added function to import Tanzania system output, `structure_tza()`
* Added an indicator mapping for Tanznaia, `data(ind_map_tza)`, with full list of collected indicators and ones that we are currently collecting for OHA, breaking out disaggs.
* Added a `NEWS.md` file to track changes to the package.
