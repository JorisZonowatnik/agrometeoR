
# AgrometeoR

* install the development version from R : 
```devtools::install_github("pokyah/agrometeoR")```
* add your API key into your `~/.Renviron` file ==> `AGROMET_API_V1_KEY = <YOUR_TOKEN>`
* detailed tutorials available in articles section

## Introduction

The aim of the Agromet project is to provide a near real-time hourly gridded datasets of weather parameters at the resolution of 1 km² for the whole region of Wallonia characterized by a quality indicator. This package constitutes the core of the spatialization process of the weather data. It makes an heavy use of [mlr](https://mlr.mlr-org.com/index.html) package and its unified interface.

## Features

* query the agromet DB using an API
* spatialize an hourly our daily (soon) dataset using a specified learner and output a geojson file
* conduct benchmark experiment to compare and rank various learners, hyperparameters and features (explanatory variables) according to your desired performance metrics
* quickly create an interactive leaflet map for data vizualization (soon)
* an interactive tutorial about R as a GIS

## target variables

The target weather parameter values at the stations are 

## explanatory variables

### static

All the static explanatory variables were prepared both for the stations locations and the RMI inca grid points locations. The script used to generate these datasets is available under `./data-raw/makeData.R`

The currently available static explanatory variables are :

`elevation`, `aspect`, `slope`, `Y` (lat), `X` (lon), `Agricultural_areas`, `Artificials_surfaces`, `Forest`,  `Herbaceous_vegetation`, `Water`

The script used to generate the static explanatory variables datasets (`grid.df` and `stations.df`) are available under `./raw-data/makeData.R` 

### dynamic 

These are not yet implemented as they require a working API to make queries in our Agromet DB.

## learners

The package will allow you to use a configuration where you can list the various learning algorithms you want to use to spatialize data









