# wildfire-exposure-behavior-public
Repo supporting Burke et al 2022 "Exposures and behavioral responses to wildfire smoke".

Results from the paper are in the `figures` and `tables` folders. Code to replicate results are in the `scripts` folder. Data are in [Dropbox](https://www.dropbox.com/sh/1q6ed2wu23wxv3m/AAC1iNXoisapYkyckFW8opKSa?dl=0).

## How to replicate results
1. Download this repository.
2. Download data from [Dropbox](https://www.dropbox.com/sh/1q6ed2wu23wxv3m/AAC1iNXoisapYkyckFW8opKSa?dl=0).
3. Change settings in `scripts/00_03_load_settings.R`:
    1. Set `path_dropbox` to the location of the data downloaded from Dropbox.
    2. Set `path_github` to the location of this repository's root.
    3. Set `key` to the value of your US Census Bureau API Key (which can be requested [here](https://api.census.gov/data/key_signup.html)).
4. Install packages by running `scripts/00_00_install_packages.R`.
5. Run `scripts/run_all.R`.

## Data
Certain data cannot be redistributed or are too large and thus are not included in the Dropbox folder or are provided only in processed form:
* Twitter
* SafeGraph
* SEDAC Population
* CoreLogic
* DMA
* PurpleAir
* ERA5
* IPUMS ATUS
