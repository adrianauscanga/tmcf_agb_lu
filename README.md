### tmcf_agb_lu

# Incorporating small-scale disturbances in models of forest structure and aboveground biomass of tropical mountains

Data and code for analyzing the effect of recent small-scale disturbances on forest structure and aboveground biomass (AGB) in a Mexican Tropical Montane Cloud Forest using Landsat time series and forest inventory data.
This repository contains scripts for organizing, analyzing, and visualizing data.
Landsat time series were obtained from Google Earth Engine.
Forest structure data were obtained from the Mexican Forest Inventory (FI) database. To see how FI dataset was processed please refer to [this repository](https://github.com/adrianauscanga/nmo_cloudforest_landscapes) and associated [manuscript](https://doi.org/10.1007/s10021-023-00861-1).

This repository, original (input folder) and processed data (output folder) are archive on Zenodo.
[![DOI](https://zenodo.org/badge/479808985.svg)](https://zenodo.org/badge/latestdoi/479808985)

Repository structure:

* `scripts/`
  * `00_organize_data.R` - read in and organize data, save data in output/
  * `01_rs_analysis.R` - run BFAST and analysis, save data in output/
  * `02_more_analysis.R` - run linear mixed effects models
  * `03_figures.R` - make figures
* `input/`- (ignored) raw data
* `output/`- processed data
  