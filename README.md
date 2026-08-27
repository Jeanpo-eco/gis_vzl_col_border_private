# Colombia-Venezuela Border Accessibility and Local Economic Activity

This project was developed by Jean Pierre Oliveros (jean.oliveros@uni-oldenburg.de) for the Summer Seminar 2026 of the Applied Econometrics Using GIS Techniques module.

The project studies how changes in road-network accessibility to the Colombia-Venezuela border affect local economic activity. The empirical strategy combines geographic network analysis, remote sensing data, and panel econometric methods.

The core outcome is municipal quarterly nighttime-light activity from VIIRS, and the main explanatory variable is a continuous accessibility shock defined from travel distance to operational border crossings.

## 1. Project goal

The goal of the project is to assess whether municipalities with larger losses in access to usable border crossings also experience weaker local economic activity.

The analysis is built around three main ideas:

- border restrictions affect local economies through road-network accessibility, not only through geographic proximity;
- the relevant treatment is an exposure measure based on distance to the nearest open crossing;
- nighttime lights provide a consistent quarterly proxy for activity in a setting where local macroeconomic statistics are sparse.

## 2. Main pipeline

The project is organized as a sequence of R scripts. The overall workflow is:

1. Set up the environment and install required R packages.
2. Download the raw geographic and administrative data.
3. Process and clean municipal boundaries and the border study area.
4. Identify the official vehicular crossings.
5. Build the motorcar road network for the region.
6. Download and process nighttime-light rasters.
7. Construct accessibility measures using shortest-path travel distances.
8. Build the municipality-quarter panel.
9. Estimate econometric models with fixed effects and event-study checks.
10. Run robustness analyses.
11. Generate final figures and summary tables for the results and appendix.

This is the intended execution order:

```r
source("00_master.R")
source("01_raw_data.R")
source("02_process.R")
source("03_border_cross.R")
source("04_roads.R")
source("01_raw_lights.R")
source("05_viirs_q.R")
source("06_baseline_access.R")
source("07_panel_construction.R")
source("08_econometric_estimations.R")
source("09_robustness.R")
source("10_figures_tables.R")
```

In practice, the numbered scripts are the actual analysis pipeline; `00_master.R` is the setup script that installs the packages and loads the required credentials.

## 3. Script-by-script description

### `00_master.R`

This is the project setup script.

It does the following:

- installs and loads all required R packages;
- defines the package groups used across the project;
- prepares the environment for GIS work and econometric analysis;
- stores NASA Earthdata credentials using `keyring`;
- fetches the NASA login token used by `blackmarbler`.

This script is critical because several data download steps depend on valid Earthdata access.

### `01_raw_data.R`

Downloads and saves the raw spatial input data:

- municipal boundaries from GADM for Colombia and Venezuela;
- country-level borders from `rnaturalearth`;
- OpenStreetMap road data for Colombia and Venezuela from Geofabrik;
- the raw data are saved under `Data/raw/` and `Data/maps/`.

### `02_process.R`

Processes the downloaded boundaries and defines the study area.

It does the following:

- converts municipality shapefiles into sf objects;
- harmonizes CRS and IDs;
- creates the combined study area covering municipalities near the border;
- saves the cleaned municipal data and study-area layers in `Output/maps/boundries/`.

### `03_border_cross.R`

Defines the five official vehicular crossings between Colombia and Venezuela.

Main steps:

- creates the crossing reference table;
- geocodes crossing locations;
- validates the result to ensure all five crossings are present;
- exports the crossing layer to spatial files and RDS.

### `04_roads.R`

Builds the road network used for the accessibility measure.

Main steps:

- reads the raw OSM road data;
- filters the relevant road classes;
- clips roads to the border study area;
- creates a motorcar-weighted routing network using `dodgr`;
- saves the resulting road and network layers in `Output/Bases/roads/` and `Output/network/`.

### `01_raw_lights.R` and `05_viirs_q.R`

These scripts handle the nighttime-light data.

They:

- use `blackmarbler` to query and download VIIRS nighttime-light rasters;
- handle the quarterly or annual preprocessing needed for the satellite outcome;
- crop the rasters to the study area;
- save processed rasters for downstream extraction and aggregation.

### `06_baseline_access.R`

Constructs the baseline accessibility measure.

This stage computes:

- shortest-path travel distances from municipalities to operational crossings;
- period-specific accessibility under open and closed corridor conditions;
- the municipality-level distance changes that define treatment intensity.

### `07_panel_construction.R`

Creates the final municipality-quarter panel used in the empirical analysis.

Key outputs include:

- municipality-quarter treatment intensity;
- post-treatment indicators;
- exposure variables interacted with the post period;
- distance-change measures used in the econometric models;
- the final estimation sample used in the regressions.

### `08_econometric_estimations.R`

Runs the main empirical models.

The estimate is based on a treatment intensity of the form:

```r
D_i = log(d_i_post) - log(d_i_pre)
```

and the baseline model corresponds to:

```r
log(1 + NTL_it) = beta * (D_i x Post_t) + municipality FE + time FE + error_it
```

The script progressively strengthens the time controls and reports estimations under:

- quarter FE;
- country x quarter FE;
- crossing-corridor x quarter FE;
- state/department x quarter FE.

It also computes event-study panels and checks for dynamic effects.

### `09_robustness.R`

Tests whether the results are sensitive to alternative specifications and alternative samples.

This script includes:

- buffer-robustness checks;
- alternative event-study windows;
- subgroup or country-specific checks;
- additional diagnostics for the stability of the estimated relationship.

### `10_figures_tables.R`

Produces the final figures and tables used in the presentation and appendix.

Examples include:

- main coefficient sensitivity figures;
- event-study plots;
- reopening plots;
- heterogeneity figures;
- summary tables for the appendix.

## 4. Data sources and dependencies

The project relies on several open-data and geospatial sources:

- GADM administrative boundaries for Colombia and Venezuela;
- Natural Earth country boundaries;
- OpenStreetMap road network data via Geofabrik and `osmextract`/`osmdata` workflows;
- VIIRS nighttime-light rasters via NASA Earthdata through `blackmarbler`;
- road-network data and shortest-path routing via `dodgr` and `sfnetworks`/`cppRouting`.

## 5. Required software and environment

### R version

A recent R installation is recommended, ideally R 4.3 or newer.

### Required packages

The project uses the following package groups:

#### Data manipulation

- `tidyverse`
- `stringr`

#### GIS and spatial analysis

- `modisfast`
- `terra`
- `sf`
- `giscoR`
- `geodata`
- `rnaturalearth`
- `osmdata`
- `osmextract`
- `tidygeocoder`
- `exactextractr`
- `sfnetworks`
- `cppRouting`
- `dodgr`
- `blackmarbler`

#### Utilities and econometrics

- `keyring`
- `here`
- `httr2`
- `tidygraph`
- `fixest`
- `modelsummary`
- `fs`
- `patchwork`

Most of these are installed automatically by `00_master.R`.

## 6. Requirements to work on the project

To reproduce the pipeline successfully, you need:

- a working R installation;
- internet access for downloading OSM, GADM, and VIIRS data;
- a valid NASA Earthdata account for the `blackmarbler` calls;
- credentials stored in `keyring` before running the data download steps.

The Earthdata requirement is especially important. The project expects a successful `blackmarbler::get_nasa_token()` call, which relies on valid Earthdata username and password credentials.

Typical setup pattern in the project:

```r
keyring::key_set(service = "nasa_api", username = "my_earth_data_username")
keyring::key_set(service = "nasa_api", username = "my_earth_data_password")
```

and then:

```r
nasa_username <- keyring::key_get(service = "nasa_api", username = "my_earth_data_username")
nasa_password <- keyring::key_get(service = "nasa_api", username = "my_earth_data_password")

nasa_login <- blackmarbler::get_nasa_token(username = nasa_username, password = nasa_password)
```

Without valid Earthdata credentials, the VIIRS download part will not work.

## 7. Output folders and generated artifacts

The project writes results to a structured set of output folders, including:

- `Data/raw/` — raw downloaded files;
- `Data/maps/` — processed map and boundary data;
- `Output/Bases/` — cleaned base layers and panel data;
- `Output/maps/boundries/` — study area and municipality files;
- `Output/network/` — road-network objects;
- `Output/Figures/final/` — final figures and plotting outputs;
- `Reference/` — references and supporting material.

## 8. Notes on execution

- The project is designed as a computational GIS/econometrics workflow, not a package-based application.
- The scripts are intended to be executed sequentially.
- `00_master.R` is the entry point for environment setup and credentials.
- `presentation` and `Docs` are intentionally omitted here because they are not required to run the core analysis or reproduce the results.

## 9. Practical summary

This project is a spatial panel study of border accessibility and economic activity. It combines:

- GIS road-network analysis,
- remote sensing outcomes,
- municipality-level panel construction,
- and causal/near-causal empirical estimation.

The main objective is to quantify how network access to a border crossing affects local economic activity and whether the result is credible once stronger fixed effects and dynamic checks are considered.
