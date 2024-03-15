This repository is published in Zenodo at the following DOI: 10.5281/zenodo.10822550. Please cite the Zenodo release. 

# Data and Reproducible Analysis For: "Fine-Scale Associations Between Land Cover Composition and the Oviposition Activity of Native and Invasive Aedes Vectors of La Crosse Virus"

This repository contains pre-processed data sets and code scripts to reproduce the data processing and analyses that are presented in the corresponding manuscript. Some minor pre-processing was completed before presenting this -- namely, the land cover raster was clipped to the study area of Knox County, Tennessee, USA, prior to placing in the repository to reduce the file size. 

# References for source data 

 - Some of the data in this repository were originally obtained from open access sources. 

 - Land cover data was obtained from the National Land Cover Database (NLCD) 2019 data product, specifically the "NLCD 2019 Land Cover (CONUS)" product. The original, unclipped raster can be freely downloaded here: https://www.mrlc.gov/data/nlcd-2019-land-cover-conus

 - Temperature data was downloaded from the United States National Oceanic and Atmospheric Administration (NOAA) weather station for Knoxville, Tennessee. The source data can be downloaded from this site: https://www.weather.gov/mrx/tysclimate

 - Rainfall data was obtained from the City of Knoxville rainfall data website, located here: https://www.knoxvilletn.gov/government/city_departments_offices/engineering/stormwater_engineering_division/rainfall_data

 - All mosquito collection data was collected directly by the manuscript authors

# How to use this repository to reproduce results 

This repository is designed to support the reproduction of analyses in the associated manuscript. The entire project can be downloaded and stored anywhere on your computer, as long as the file structure is not altered. The project contains folders with all data sets and code scripts necessary for analysis. 


What you will need: 
 - Installed R and RStudio for purely spatial cluster and global model analyses
 - Basic understanding of how to open R and run code 

 You do NOT need:
 - To download or install R packages on your own; that is taken care of within this environment
 - To write any code 
 - To set up any working directories in R 

## ***Important***: Using `renv`

Short Version: When you open the R project, run `renv::restore()` and follow the prompts to install the necessary R packages. 

The R package `renv` was used to create a **project library**, which contains all R packages that are used by the project. The packages in the project library are **the versions used during the original analysis**. This means that if any packages are updated by developers in ways that would change the results of the analysis, this project can still produce the original results because of `renv`. When you open this project for the first time, `renv` will automatically download and install itself and ask you to run `renv::restore()`. **You should run `renv::restore()` to automatically download and install all of the packages within this reproducible environment**. 

## Basic step-by-step guide:

- 1. Download the entire repository by clicking "Code -> Download ZIP" on GitHub or by downloading the ZIP file in Zenodo
- 2. Extract the ZIP file anywhere on your computer (do not change the structure of the files once extracted)
- 3. In RStudio, click *File -> Open Project* and browse to the location where you extracted the repository; in the repository file, open the knoxaedeslandcover R Project file 
- 4. Open any of the R scripts in the `analysis/` folder
- 5. Run the code `renv::restore()` in the script or in the console and follow the prompt to install the packages 
  - Now you can run the R Scripts; start from the top with loading the packages and data, then work your way down line-by-line


# `data/` Folder

This folder contains three datasets. Most are stored as a **GeoPackage** (.gpkg), which is a platform-independent format for storing geographic information and related attributes. These files can be opened in a variety of software, including open-source programs like R and QGIS, but also proprietary GIS software like ArcGIS Pro. See `data dictionary.txt` for a description of all attributes contained within each file. 

## Files within the `data/` folder
  - `knox22_joined.RDS` contains a cleaned and joined version of land cover, climate, and mosquito data in R Data Serialization format, which maintains predefined factor and numeric designations for columns. 
 - `knox22_joined.csv` contains a cleaned and joined version of land cover, climate, and mosquito data in CSV format -- identical to 'knox22_joined.RDS'
 - `sites22.csv` contains the names, site codes, and coordinates of the study sites
 - `aedes22_clean.csv` contains the raw mosquito collection data for the study without any climate or land cover information 
 - `NLCD_2019_landcover_clippedtoKnox.tif` contains the NLCD land cover data, already clipped to Knox County, TN, USA
 - `knox22_temperature.csv` contains raw daily temperatures for the city of Knoxville in 2022
 - `knox22_rainfall.csv` contains raw daily precipitation for the city of Knoxville watersheds in 2022
 - `rainfall_stations.csv` contains the descriptions, approximate street addresses, and geographic coordinates for rainfall monitoring sites 
 - `data dictionary.txt` file that defines column names and other data attributes for every dataset 

# `analysis/` Folder

The `analysis/` folder contains scripts for reproducing the purely spatial cluster analysis and predictor investigation from Day et al. (2024). Files with a .R extension are R scripts that should be opend with RStudio. 

## Files within the `analysis/` folder

The files are numbered in the order that they were run for the original analysis. In this case, none of the analyses are dependent on the others, so they can technically be used in any order. The numbers associated with each file describe the order that the analyses would normally be run. 

 - `(1)dataprep.R` contains the code for cleaning and combining the land cover, climate, and mosquito data -- this includes calculating the land cover percentages at different scales and calculating weekly and timelagged climate values
 - `(2)summary_analysis.R` contains code for reproducing summary data and creating graphs from the manuscript
 - `(3)variable_selection.R` contains code for asssessing collinearity and fitting models to identify the best fitting variables for each speceis
 - `(4)finalmodels.R` contains code for fitting the final models using the selected variables for each species 

# `renv/` Folder

The `renv/` folder contains bits and pieces needed for the `renv` package. Nothing should be altered in this folder. 

# Other miscelleanous files

Several other files are present in the root directory. None of these need to be manipulated. The only file you will actively use is the R Project file, which will open the project in RStudio and automatically set the relative paths based on the file structure of this repository. 
