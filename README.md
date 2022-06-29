# wildfire-exposure-behavior-public
Repo supporting Burke et al 2022 "Exposures and behavioral responses to wildfire smoke".

Results from the paper are in the `figures/clean` and `tables/clean` folders. Code to replicate results are in the `scripts` folder. Data are in [Dropbox](https://www.dropbox.com/sh/1q6ed2wu23wxv3m/AAC1iNXoisapYkyckFW8opKSa?dl=0).

## How to replicate results
1. Download this repository.
2. Download data from [Dropbox](https://www.dropbox.com/sh/1q6ed2wu23wxv3m/AAC1iNXoisapYkyckFW8opKSa?dl=0). Ensure that the data directory is structured according to the minimum necessary starting data directory structure described below.
3. Change settings in `scripts/setup/00_load_settings.R`:
    1. Set `key` to the value of your US Census Bureau API Key (which can be requested [here](https://api.census.gov/data/key_signup.html)).
    2. Set `num_cores` to the number of cores to use in parallel computing.
    3. Set `path_dropbox` to the location of the data downloaded from Dropbox.
    4. Set `path_github` to the location of this downloaded repository's root.
4. Install packages by running `scripts/setup/00_install_packages.R`.
5. Set working directory to this downloaded repository's root.
6. Run `scripts/run_all.R`. As noted below, certain scripts may require relatively large computer memory.

## Data
The following data are provided in raw or processed form:
* US Census Bureau TIGER/Line 2019 state, county, and tract shapefiles are downloaded from [here](https://www.census.gov/cgi-bin/geo/shapefiles/index.php) and provided in raw form, as well as loaded using the R package [tigris](https://cran.r-project.org/web/packages/tigris/) in certain scripts.
* United States Geological Survey (USGS) Physiographic Divisions shapefile is described [here](https://water.usgs.gov/GIS/metadata/usgswrd/XML/physio.xml) and downloaded from [here](https://water.usgs.gov/GIS/dsdl/physio_shp.zip) and provided in raw form.
* Environmental Protection Agency (EPA) PM2.5 data at hourly and daily resolution are obtained as [pre-generated files](https://aqs.epa.gov/aqsweb/airdata/download_files.html#Raw) and from the [EPA download portal](https://www.epa.gov/outdoor-air-quality-data/download-daily-data), respectively. Hourly data are provided in raw form, and daily data are provided in processed form, as described in the GitHub repository [wildfire-map-public](https://github.com/echolab-stanford/wildfire-map-public) which supports [Burke et al (2021)](https://doi-org.stanford.idm.oclc.org/10.1073/pnas.2011048118).
* National Oceanic and Atmospheric Administration (NOAA) Hazard Mapping System (HMS) fire points and smoke plumes are obtained from the HMS Fire and Smoke Product [archive](https://satepsanone.nesdis.noaa.gov/pub/volcano/FIRE/HMS_ARCHIVE/) and [backup](https://satepsanone.nesdis.noaa.gov/pub/FIRE/HMS/hms_backup/). Fire data are provided in raw form, and smoke data are provided in processed form, as described in [wildfire-map-public](https://github.com/echolab-stanford/wildfire-map-public).
* Google Trends search query data are retrieved using the R package [gtrendsR](https://github.com/PMassicotte/gtrendsR). Google Trends provides data in the form of aggregated non-reproducibly random samples of Google searches. Search query data are provided in processed form, which result from running `supplementary/process_google_trends.R`.
* Twitter sentiment data are collected in accordance with the terms and conditions specified in Twitter's Development Agreement and provided in processed form.
* PurpleAir PM2.5 data are downloaded from [PurpleAir servers](https://thingspeak.com/) via JSON in accordance with PurpleAir terms and conditions and provided in processed form.
* Parameter-elevation Regressions on Independent Slopes Model (PRISM) temperature data are provided in both raw form and processed form, which result from processing temperature data into cooling degree days and heating degree days using Google Earth Engine (GEE) after [registration](https://earthengine.google.com/signup/) by running `supplementary/process_tracts.R` and `supplementary/process_cdd_hdd.js`.
* European Centre for Medium-Range Weather Forecasts (ECMWF) Reanalysis 5th Generation (ERA5) Land meteorological data are retrieved at the daily level using the [Daily statistics calculated from ERA5 data tool](https://cds.climate.copernicus.eu/cdsapp#!/software/app-c3s-daily-era5-statistics?tab=app) via the [Climate Data Store API](https://cds.climate.copernicus.eu/api-how-to) after [registration](https://cds.climate.copernicus.eu/user/register) and are provided in processed form, which can be reproduced by running `supplementary/process_ERA5.R`.
* Designated Market Area (DMA) shapefile is not provided, but processed data files created using it are included in Dropbox.
* US Census Bureau American Community Survey (ACS) socioeconomic and demographic data are retrieved using the R package [tidycensus](https://cran.r-project.org/web/packages/tidycensus/) and require a US Census Bureau API Key (which can be requested [here](https://api.census.gov/data/key_signup.html)). Some but not all data are provided in processed form.

The following data are not included in Dropbox due to sharing restrictions:
* Socioeconomic Data and Applications Center (SEDAC) Gridded Population of the World (GPW) 2.5-minute population data for 2010 and 2015 require [registration](https://sedac.ciesin.columbia.edu/user-registration) and are not provided but can be downloaded [here](https://sedac.ciesin.columbia.edu/data/set/gpw-v4-population-density-rev11/data-download).
* Integrated Public Use Microdata Series (IPUMS) American Time Use Survey (ATUS) data are obtained as a microdata extract which cannot be redistributed and are not provided.
* SafeGraph mobility data are proprietary and are not provided.
* CoreLogic residential real estate data are proprietary and are not provided.

## Minimum necessary starting data directory structure
\[Square brackets\] refer to multiple files following the same naming structure by the pattern in (parentheses). Data that are not provided in Dropbox due to sharing restrictions are in **bold**.

<pre>
wildfire-exposure-behavior-public
├── <b>ATUS</b>
│   ├── <b>atus_00006.dat</b>
│   └── <b>atus_00006.xml</b>
├── EPA
│   ├── epa_smoke_clean.rds
│   ├── epa_station_level_pm25_data.rds
│   └── hourly_epa
│       └── hourly_88101_[Year].csv (for 2006 to 2020)
├── ERA5
│   ├── 2m_temperature
│   │   └── grid_temperature
│   │       └── grid_2m_temperature_daily_mean_[Year_Month].rds (for 2006_01 to 2020_12)
│   └── total_precipitation
│       └── grid_precipitation
│           └── grid_total_precipitation_daily_maximum_[Year_Month].rds (for 2006_01 to 2020_12)
├── Google Trends
│   ├── google_trends_smoke_DMA_normalized_complete.RDS
│   ├── google_trends_smoke_DMA_normalized_with_covariates.RDS
│   └── keyword
│       ├── google_trends_smoke_DMA_normalized_keyword_USWNT.RDS
│       ├── google_trends_smoke_DMA_normalized_keyword_air filter.RDS
│       ├── google_trends_smoke_DMA_normalized_keyword_air pollution.RDS
│       ├── google_trends_smoke_DMA_normalized_keyword_air purifier.RDS
│       ├── google_trends_smoke_DMA_normalized_keyword_air quality.RDS
│       ├── google_trends_smoke_DMA_normalized_keyword_calidad del aire.RDS
│       ├── google_trends_smoke_DMA_normalized_keyword_contaminacion del aire.RDS
│       ├── google_trends_smoke_DMA_normalized_keyword_dinosaurios.RDS
│       ├── google_trends_smoke_DMA_normalized_keyword_dinosaurs.RDS
│       ├── google_trends_smoke_DMA_normalized_keyword_filtro de aire.RDS
│       ├── google_trends_smoke_DMA_normalized_keyword_fire.RDS
│       ├── google_trends_smoke_DMA_normalized_keyword_floods.RDS
│       ├── google_trends_smoke_DMA_normalized_keyword_fuego.RDS
│       ├── google_trends_smoke_DMA_normalized_keyword_humo de incendios forestales.RDS
│       ├── google_trends_smoke_DMA_normalized_keyword_humo.RDS
│       ├── google_trends_smoke_DMA_normalized_keyword_huracanes.RDS
│       ├── google_trends_smoke_DMA_normalized_keyword_hurricanes.RDS
│       ├── google_trends_smoke_DMA_normalized_keyword_incendio.RDS
│       ├── google_trends_smoke_DMA_normalized_keyword_inundaciones.RDS
│       ├── google_trends_smoke_DMA_normalized_keyword_mascara antihumo.RDS
│       ├── google_trends_smoke_DMA_normalized_keyword_purificador de aire.RDS
│       ├── google_trends_smoke_DMA_normalized_keyword_purple air.RDS
│       ├── google_trends_smoke_DMA_normalized_keyword_smoke mask.RDS
│       ├── google_trends_smoke_DMA_normalized_keyword_smoke.RDS
│       ├── google_trends_smoke_DMA_normalized_keyword_steph curry.RDS
│       ├── google_trends_smoke_DMA_normalized_keyword_wildfire smoke.RDS
│       ├── google_trends_smoke_DMA_normalized_keyword_wildfire.RDS
│       ├── google_trends_smoke_DMA_normalized_keyword_火.RDS
│       ├── google_trends_smoke_DMA_normalized_keyword_口罩.RDS
│       ├── google_trends_smoke_DMA_normalized_keyword_山火.RDS
│       ├── google_trends_smoke_DMA_normalized_keyword_烟雾.RDS
│       ├── google_trends_smoke_DMA_normalized_keyword_空气质量.RDS
│       ├── google_trends_smoke_DMA_normalized_keyword_空气净化器.RDS
│       └── google_trends_smoke_DMA_normalized_keyword_空气过滤器.RDS
├── PRISM
│   ├── hourly_prism.rds
│   ├── prism_grid
│   │   └── PRISM_tmin_early_4kmD2_20201001_bil
│   │       ├── PRISM_tmin_early_4kmD2_20201001_bil.bil
│   │       ├── PRISM_tmin_early_4kmD2_20201001_bil.bil.aux.xml
│   │       ├── PRISM_tmin_early_4kmD2_20201001_bil.hdr
│   │       ├── PRISM_tmin_early_4kmD2_20201001_bil.info.txt
│   │       ├── PRISM_tmin_early_4kmD2_20201001_bil.prj
│   │       ├── PRISM_tmin_early_4kmD2_20201001_bil.stn.csv
│   │       ├── PRISM_tmin_early_4kmD2_20201001_bil.stx
│   │       └── PRISM_tmin_early_4kmD2_20201001_bil.xml
│   └── temperature
│       ├── tract_level_cdd_[Year].csv (for 2000 to 2020)
│       └── tract_level_hdd_[Year].csv (for 2000 to 2020)
├── PurpleAir
│   ├── PA_locations_all.rds
│   ├── analysis_data_clean_all.rds
│   ├── bay_area_fig4_wf_indoor_outdoor_pm.rds
│   ├── indoor_monitor_data_clean.rds
│   ├── locationTypeCategorizing
│   │   └── locationTypeCategorized.csv
│   ├── outdoor_monitor_data_clean.rds
│   ├── outdoor_monitor_data_clean_part1.rds
│   ├── purpleAir_meanOutdoorPM_by_indoorMonitor.rds
│   ├── smoke_by_PAmonitor.rds
│   ├── smoke_by_PAmonitor_density.rds
│   └── us_sensor_list.rds
├── <b>SafeGraph</b>
│   └── <b>safegraph_completely_home_ALL.rds</b>
├── Twitter
│   └── county-sentiment.fst
├── boundaries
│   ├── ca_county_boundaries.rds
│   ├── physio_shp
│   │   ├── physio.dbf
│   │   ├── physio.prj
│   │   ├── physio.sbn
│   │   ├── physio.sbx
│   │   ├── physio.shp
│   │   ├── physio.shp.xml
│   │   └── physio.shx
│   ├── stateFIPS_epaREGION_crosswalk.rds
│   ├── tl_2019_us_county
│   │   ├── tl_2019_us_county.cpg
│   │   ├── tl_2019_us_county.dbf
│   │   ├── tl_2019_us_county.prj
│   │   ├── tl_2019_us_county.shp
│   │   ├── tl_2019_us_county.shp.ea.iso.xml
│   │   ├── tl_2019_us_county.shp.iso.xml
│   │   └── tl_2019_us_county.shx
│   ├── tl_2019_us_state
│   │   ├── tl_2019_us_state.cpg
│   │   ├── tl_2019_us_state.dbf
│   │   ├── tl_2019_us_state.prj
│   │   ├── tl_2019_us_state.shp
│   │   ├── tl_2019_us_state.shp.ea.iso.xml
│   │   ├── tl_2019_us_state.shp.iso.xml
│   │   └── tl_2019_us_state.shx
│   └── tracts
│       └── tl_2019_[StateFIPS]_tract (for 01 to 02, 04 to 06, 08 to 13, 15 to 42, 44 to 51, 53 to to 56, 60, 66, 69, 72, 78)
│           ├── tl_2019_[StateFIPS]_tract.cpg
│           ├── tl_2019_[StateFIPS]_tract.dbf
│           ├── tl_2019_[StateFIPS]_tract.prj
│           ├── tl_2019_[StateFIPS]_tract.shp
│           ├── tl_2019_[StateFIPS]_tract.shp.ea.iso.xml
│           ├── tl_2019_[StateFIPS]_tract.shp.iso.xml
│           └── tl_2019_[StateFIPS]_tract.shx
├── fire
│   ├── distance_to_fire
│   │   └── dma_weekly_dist_to_fire_cluster.RDS
│   ├── hms_fires.RDS
│   └── points
│       ├── hms_fire[YearMonthDay].dbf (for 20060101 to 20201231)
│       ├── hms_fire[YearMonthDay].shp (for 20060101 to 20201231)
│       └── hms_fire[YearMonthDay].shx (for 20060101 to 20201231)
├── infiltration
│   ├── bootstraps
│   │   ├── infiltration_smoke_income_bootstrap_run_results_linear_smokeday.rds
│   │   └── infiltration_smoke_pm_bootstrap_run_results_smokeday.rds
│   ├── estimates
│   │   ├── PA_monitor_level_infiltration_estimates_sfr_clean.rds
│   │   ├── PA_monitor_level_infiltration_estimates_sfr_clean_pc.rds
│   │   ├── PA_monitor_level_infiltration_estimates_sfr_clean_uncorrected.rds
│   │   └── purpleair_infiltration_estimates_by_model.rds
│   └── heterogeneity
│       └── <b>CoreLogic</b>
│           ├── <b>matches_cl_ctf.rds</b>
│           └── <b>matches_cl_nn.rds</b>
├── <b>population</b>
│   ├── <b>gpw_v4_population_count_rev11_2010_2pt5_min.tif</b>
│   └── <b>gpw_v4_population_count_rev11_2015_2pt5_min.tif</b>
├── smoke
│   └── smoke_plumes_spdf.RDS
└── smokePM
    ├── panel_dma_pm_smoke_day_weekly.RDS
    └── smokePM_forMarshall.rds
</pre>

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

R packages versions are specified in `scripts/setup/00_install_packages.R`.

Certain scripts may require relatively large computer memory:
* `main/12_figure04.R`
* `main/13_figureED01.R`
* `main/19_figureED07.R`
* `main/21_figureED09.R`
* `main/25_tableS12.R`
