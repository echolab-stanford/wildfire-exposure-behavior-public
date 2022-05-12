# wildfire-exposure-behavior-public
Repo supporting Burke et al 2022 "Exposures and behavioral responses to wildfire smoke".

Results from the paper are in the `figures` and `tables` folders. Code to replicate results are in the `scripts` folder. Data are in [Dropbox](https://www.dropbox.com/sh/1q6ed2wu23wxv3m/AAC1iNXoisapYkyckFW8opKSa?dl=0).

## How to replicate results
1. Download this repository.
2. Download data from [Dropbox](https://www.dropbox.com/sh/1q6ed2wu23wxv3m/AAC1iNXoisapYkyckFW8opKSa?dl=0).
3. Change settings in `scripts/00_03_load_settings.R`:
    1. Set `path_dropbox` to the location of the data downloaded from Dropbox.
    2. Set `path_github` to the location of this downloaded repository's root.
    3. Set `key` to the value of your US Census Bureau API Key (which can be requested [here](https://api.census.gov/data/key_signup.html)).
4. Set working directory to this downloaded repository's root.
5. Install packages by running `scripts/00_00_install_packages.R`.
6. Run `scripts/run_all.R`.

## Data
* US Census Bureau TIGER/Line 2019 shapefiles at the tract, county, and state level are downloaded from [here](https://www.census.gov/cgi-bin/geo/shapefiles/index.php).
* Environmental Protection Agency (EPA) PM2.5 data at daily and hourly resolution are obtained from the [EPA download portal](https://www.epa.gov/outdoor-air-quality-data/download-daily-data) and as [pre-generated files](https://aqs.epa.gov/aqsweb/airdata/download_files.html#:~:text=2017%2D05%2D26-,Particulates,-Year), respectively.
* National Oceanic and Atmospheric Administration (NOAA) Hazard Mapping System (HMS) smoke plumes and fire points are obtained from the HMS Fire and Smoke Product [archive](https://satepsanone.nesdis.noaa.gov/pub/volcano/FIRE/HMS_ARCHIVE/) and [backup](https://satepsanone.nesdis.noaa.gov/pub/FIRE/HMS/hms_backup/).
* Google Trends query data are retrieved using the R package `gtrendsR`. Google Trends provides data in non-reproducibly random samples of Google searches.
* United States Geological Survey (USGS) Physiographic Divisions shapefile is described [here](https://water.usgs.gov/GIS/metadata/usgswrd/XML/physio.xml) and can be downloaded [here](https://water.usgs.gov/GIS/dsdl/physio_shp.zip).
* EPA regions

Certain data cannot be redistributed or are too large. These data are either not included in the Dropbox folder or are provided only in processed form:
* Twitter
* SafeGraph
* Socioeconomic Data and Applications Center (SEDAC) population data require [registration](https://sedac.ciesin.columbia.edu/user-registration) and can be downloaded [here](https://sedac.ciesin.columbia.edu/data/set/gpw-v4-population-density-rev11/data-download).
* CoreLogic property tax records
* DMA shapefile
* PurpleAir PM2.5
* European Centre for Medium-Range Weather Forecasts (ECMWF) Reanalysis 5th Generation (ERA5) Land meteorological data are retrieved at the daily level using the [Daily statistics calculated from ERA5 data tool](https://cds.climate.copernicus.eu/cdsapp#!/software/app-c3s-daily-era5-statistics?tab=app) via the [Climate Data Store API](https://cds.climate.copernicus.eu/api-how-to) after [registration](https://cds.climate.copernicus.eu/user/register).
* IPUMS American Time Use Survey
* US Census Bureau American Community Survey socioeconomic and demographic data are retrieved using the R package `tidycensus` and require a US Census Bureau API Key (which can be requested [here](https://api.census.gov/data/key_signup.html)).
* Parameter-elevation Regressions on Independent Slopes Model (PRISM) temperature data are retrieved and processed into cooling degree days and heating degree days using Google Earth Engine, which requires [registration](https://earthengine.google.com/signup/).

## Computational environment
```
R version 4.0.4 (2021-02-15)
Platform: x86_64-apple-darwin17.0 (64-bit)
Running under: macOS Catalina 10.15.7

Matrix products: default
BLAS: /System/Library/Frameworks/Accelerate.framework/Versions/A/Frameworks/vecLib.framework/Versions/A/libBLAS.dylib
LAPACK: /Library/Frameworks/R.framework/Versions/4.0/Resources/lib/libRlapack.dylib

GEOS 3.9.1
GDAL 3.3.1
PROJ 8.1.0
ImageMagick 6.9.12.3
```

R packages versions are specified in `scripts/00_00_install_packages.R`.
