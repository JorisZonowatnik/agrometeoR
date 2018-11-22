
## Aims

This package aims to make weather spatilization of an hourly record of a passed weather parameter based on the records stored by the PAMESEB automatic weather station network. As output this function produces a json file containing the spatialized data. You can then use it with e.g. a leaflet map.

## Internal datasets

The functions of the package make heavy use of internal datasets : 
* wallonia.sf => polygon that contains the borders of wallonia
* grid.df => a dataframe containing the static explanatory variables @ grid points
* stations.df => a dataframe containing the static explanatory variables @ stations points
* grid.sf => a sf dataframe containing the station locations
* stations.sf => a sf dataframe containing the grid points locations

# CRS
Lambert 2008 - EPSG = 3812

The script used to generate these objects is available under `./raw-data/makeData.R` 

# How to install the package ?

Within R :
`devtools::install_github("pokyah/agrometeoR", ref="master")`





