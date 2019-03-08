#####
## LIBRARIES


library(tidyverse)
library(lubridate)
library(rgdal)
library(rgeos)
library(dplyr)
library(sf)
library(raster)
library(rnaturalearth)
library(rnaturalearthhires)
library(jsonlite)
library(elevatr)
source("./R/agrometAPI.R")


#####
## ADMIN BOUNDARIES

# loading wallonia boundaries from local geojson file created by JP huart and removing all attributes
wallonia = sf::st_read(dsn = "./data-raw/extdata/AGROMET/wallonie.geojson")
wallonia = wallonia[,-(1:length(wallonia))]

#####
## DEM

# downloading the raster tile data using elevatr
# for resolution corresponding to z parameter check elevatr doc https://cran.r-project.org/web/packages/elevatr/vignettes/introduction_to_elevatr.html#get_raster_elevation_data
# 9 is around 200 m resolution at 45° lat
DEM = elevatr::get_elev_raster(
  as(
    sf::st_transform(
      sf::st_buffer(sf::st_transform(wallonia, 3812), 5000),
      4326),
    "Spatial"),
  z = 9,
  src = "aws")
# cliping to bbox of Wallonia + 5km buffer
DEM = raster::mask(DEM,
  as(
    sf::st_transform(
      sf::st_buffer(sf::st_transform(wallonia, 3812), 5000),
      4326),
    "Spatial"))
elevation = DEM
# computing slope and aspect
slope = raster::terrain(DEM, "slope")
aspect = raster::terrain(DEM, "aspect")


#####
## CORINE LAND COVER


# Download CORINE land cover for Belgium from http://inspire.ngi.be/download-free/atomfeeds/AtomFeed-en.xml
# read the downlaoded .shp
corine <- sf::st_read("./data-raw/extdata/CLC/CLC12_BE.shp")
# project to 4326
corine = sf::st_transform(corine, 4326)
# crop to Wallonia + 5 km buffer
corine = sf::st_intersection(sf::st_transform(corine, 3812), sf::st_buffer(sf::st_transform(wallonia, 3812), 5000))
# Download legend for CLC
download.file("http://www.eea.europa.eu/data-and-maps/data/corine-land-cover-2006-raster-1/corine-land-cover-classes-and/clc_legend.csv/at_download/file",
  destfile = "./data-raw/extdata/CLC/clc_legend.csv")
corine.lgd = read.csv(file = "./data-raw/extdata/CLC/clc_legend.csv", header = TRUE, sep = ",")
corine.lgd$CLC_CODE = as.numeric(corine.lgd$CLC_CODE)
# Legend codes present in Wallonia
lgd.codes = data.frame(unique(corine$code_12))
colnames(lgd.codes) = "CLC_CODE"
lgd.codes$CLC_CODE = as.numeric(as.character(lgd.codes$CLC_CODE))
# Legend for CLC in Wallonia
wal.lgd = corine.lgd %>%
  dplyr::filter(CLC_CODE %in% lgd.codes$CLC_CODE)
# Reclass all types of CLC to create 6 groups
corine$code_12 = as.numeric(as.character(corine$code_12))
corine = corine %>%
  dplyr::mutate(
    custom.class = dplyr::case_when(
      code_12 <= 142 ~ "Artificials surfaces",
      code_12 == 211 ~ "Agricultural areas",
      code_12 == 222 ~ "Agricultural areas",
      code_12 == 231 ~ "Herbaceous vegetation",
      code_12 == 242 ~ "Agricultural areas",
      code_12 == 243 ~ "Agricultural areas",
      code_12 == 311 ~ "Forest",
      code_12 == 312 ~ "Forest",
      code_12 == 313 ~ "Forest",
      code_12 == 321 ~ "Herbaceous vegetation",
      code_12 == 322 ~ "Herbaceous vegetation",
      code_12 == 324 ~ "Forest",
      code_12 > 400 ~ "Water")
  )
corine = sf::st_transform(corine, 4326)

#####
## RMI INCA GRID

# load the INCAgrid + buffer 5km built from january 2019 RMI data (with AGROMET px refs by jphuart)
incaGrid_pxAg = sf::st_read("./data-raw/extdata/AGROMET/agromet_inca_grid_buf_5km.geojson")

# rename
incaGrid = incaGrid_pxAg

#####
## INCA GRID EXTRACTIONS : DEM

# extracting DEM
inca.elevation.ext <- raster::extract(
  raster::projectRaster(
    DEM,
    crs = (sf::st_crs(incaGrid))$proj4string
  ),
  as(incaGrid, "Spatial"),
  buffer = 500, # the buffer on which calculate the mean elevation around grid centroids
  fun = mean,
  na.rm = TRUE,
  df = TRUE
)
# rename column layer to elevation
colnames(inca.elevation.ext) = c("ID", "elevation")
# extracting slope
inca.slope.ext <- raster::extract(
  raster::projectRaster(
    slope,
    crs = (sf::st_crs(incaGrid))$proj4string
  ),
  as(incaGrid, "Spatial"),
  buffer = 500,
  fun = mean,
  na.rm = TRUE,
  df = TRUE
)
# extracting aspect
inca.aspect.ext <- raster::extract(
  raster::projectRaster(
    aspect,
    crs = (sf::st_crs(incaGrid))$proj4string
  ),
  as(incaGrid, "Spatial"),
  buffer = 500,
  fun = mean,
  na.rm = TRUE,
  df = TRUE
)
# storing in a sf object
inca.ext = bind_cols(incaGrid, inca.elevation.ext, inca.slope.ext, inca.aspect.ext)

#####
## INCA GRID EXTRACTIONS : CLC

# Make a 500m radius buffer around  points for CLC extract
inca.buff.grd = sf::st_buffer(sf::st_transform(incaGrid, 3812), dist = 500)
# inca.buff.grd = incaGrid # the grid is already buffered by JP Huart

# extract cover information into the buffered points
corine.inca = sf::st_intersection(inca.buff.grd, sf::st_transform(corine, 3812))
# create an id for each buffer
corine.inca <- corine.inca %>%
  dplyr::mutate(
    bid = paste0(seq_along(1:nrow(corine.inca))))
# create new column with area of each intersected cover polygon
corine.inca.area.grd <- corine.inca %>%
  dplyr::group_by(bid) %>%
  dplyr::summarise() %>%
  mutate(shape.area = st_area(.))
# Make a column with percentage of occupation of each land cover inside each grid point buffer
# https://github.com/r-spatial/sf/issues/239
cover.rate <- sf::st_join(
  x = corine.inca,
  y = corine.inca.area.grd,
  join = sf::st_covered_by
) %>%
  dplyr::select(px, custom.class, shape.area) %>%
  dplyr::mutate(cover_rate = as.numeric(shape.area)/(pi*500^2) * 100) #500 = buffer radius
# transposing to dataframe for data spreading (impossible (?) to achieve with dplyr spread)
cover2df = function(data.sf) {
  # Delete geometry column
  data.df <- data.frame(data.sf)
  # Reshape data with CLASS labels as columns names
  # https://stackoverflow.com/questions/39053451/using-spread-with-duplicate-identifiers-for-rows
  data.df <- data.df %>%
    dplyr::select(px, custom.class, cover_rate) %>%
    reshape2::dcast(px ~ custom.class, fun = sum)
  return(data.df)
}
cover.rate = cover2df(cover.rate)
# replacing white space by underscores
colnames(cover.rate) <- gsub(" ","_",colnames(cover.rate))
# merge cover data with grid
inca.ext = merge(inca.ext, cover.rate, by = "px")
# removing duplicated ID cols resulting from bind_cols operation
excluded_vars = c("ID1", "ID2")
inca.ext  = dplyr::select(inca.ext, -one_of(excluded_vars))

#####
## FINALISING THE GRID

# static independent vars at grid points + grid points geography objects
grid.sf = inca.ext

# adding lat/lon data from from grid.sf to grid & limiting to Wallonia without buffer
grid.sf = sf::st_intersection(sf::st_transform(grid.sf, 3812), sf::st_transform(wallonia, 3812))

grid.df = grid.sf

grid.sf = grid.sf %>%
  dplyr::select(c(px))

grid.df = grid.df %>%
  dplyr::select(c(px, elevation, slope, aspect, Agricultural_areas, Artificials_surfaces, Forest, Herbaceous_vegetation))

grid.df = grid.df %>%
  dplyr::left_join(
    (data.frame(st_coordinates(st_transform(grid.sf, 3812))) %>%
        dplyr::bind_cols(grid.sf["px"]) %>%
        dplyr::select(-geometry)
    ),
    by = "px"
  )
sf::st_geometry(grid.df) = NULL

# exporting the grid to geojson file
grid.data.sf = grid.sf %>% left_join(grid.df, by = "px")
sf::st_write(grid.data.sf, dsn = "./data-raw/grid.geojson", driver = "GeoJSON")

# the points grid as square polygons for map rendering
grid.squares.sf = sf::st_make_grid(
  x = grid.sf, cellsize = 1000, what = "polygons",
  offset = c(min(grid.df$X), min(grid.df$Y)))

grid.squares.sf = sf::st_intersection(sf::st_transform(grid.squares.sf, 3812), sf::st_transform(wallonia, 3812))
grid.squares.sf = sf::st_join(sf::st_transform(sf::st_sf(grid.squares.sf), 3812), sf::st_transform(grid.sf, 3812))
grid.squares.sf = na.omit(grid.squares.sf)

#####
## SAVING ALL THE GRID + WALLONIA OBJECTS TO PACKAGE DATA
# doc : http://r-pkgs.had.co.nz/data.html

# saving in ./data/*.rda
devtools::use_data(wallonia, grid.sf, grid.df, grid.squares.sf, internal = FALSE, overwrite = TRUE)


#####
## STATIONS STATIC FEATURE EXTRACTION

stations = typeData(meta_and_records.l = getData(dfrom = "2018-01-01T00:00:00Z", dto = "2018-01-01T00:01:00Z"), table_name = "cleandata")
stations = stations %>%
  dplyr::filter(type_name != "Sencrop") %>%
  dplyr::filter(state == "Ok") %>%
  dplyr::select(c("sid", "poste", "longitude", "latitude", "network_name", "type_name"))

stations = st_as_sf(stations, coords = c("longitude", "latitude"))
stations = sf::st_set_crs(stations, 4326)

# extracting DEM
stations.DEM.ext <- raster::extract(
  raster::projectRaster(
    DEM,
    crs = (sf::st_crs(stations))$proj4string
  ),
  as(stations, "Spatial"),
  buffer = 200,
  fun = mean,
  na.rm = TRUE,
  df = TRUE
)
# rename column layer to elevation
colnames(stations.DEM.ext) = c("ID", "elevation")

# extracting slope
stations.slope.ext <- raster::extract(
  raster::projectRaster(
    slope,
    crs = (sf::st_crs(stations))$proj4string
  ),
  as(stations, "Spatial"),
  buffer = 200,
  fun = mean,
  na.rm = TRUE,
  df = TRUE
)

# extracting aspect
stations.aspect.ext <- raster::extract(
  raster::projectRaster(
    aspect,
    crs = (sf::st_crs(stations))$proj4string
  ),
  as(stations, "Spatial"),
  buffer = 200,
  fun = mean,
  na.rm = TRUE,
  df = TRUE
)

# storing in a sf object
stations.ext = bind_cols(stations, stations.DEM.ext, stations.slope.ext, stations.aspect.ext)

# Make a 200m radius buffer around  points for CLC extract
stations.buff = sf::st_buffer(sf::st_transform(stations, 3812), dist = 200)

# extract cover information into the buffered points
corine.stations = sf::st_intersection(stations.buff, sf::st_transform(corine, 3812))
# create an id for each buffer
corine.stations <- corine.stations %>%
  dplyr::mutate(
    bid = paste0(seq_along(1:nrow(corine.stations))))
# create new column with area of each intersected cover polygon
corine.stations.area <- corine.stations %>%
  dplyr::group_by(bid) %>%
  dplyr::summarise() %>%
  mutate(shape.area = st_area(.))
# Make a column with percentage of occupation of each land cover inside each grid point buffer
# https://github.com/r-spatial/sf/issues/239
cover.rate <- sf::st_join(
  x = corine.stations,
  y = corine.stations.area,
  join = sf::st_covered_by
) %>%
  dplyr::select(sid, custom.class, shape.area) %>%
  dplyr::mutate(cover_rate = as.numeric(shape.area)/(pi*500^2) * 100) #500 = buffer radius
# transposing to dataframe for data spreading (impossible (?) to achieve with dplyr spread)
cover2df = function(data.sf) {
  # Delete geometry column
  data.df <- data.frame(data.sf)
  # Reshape data with CLASS labels as columns names
  # https://stackoverflow.com/questions/39053451/using-spread-with-duplicate-identifiers-for-rows
  data.df <- data.df %>%
    dplyr::select(sid, custom.class, cover_rate) %>%
    reshape2::dcast(sid ~ custom.class, fun = sum)
  return(data.df)
}
# transposing to dataframe for data spreading (impossible (?) to achieve with dplyr spread)
cover.rate = cover2df(cover.rate)
# replacing white space by underscores
colnames(cover.rate) <- gsub(" ","_",colnames(cover.rate))
# merge cover data with stations
stations.ext = merge(stations.ext, cover.rate, by = "sid")
# removing duplicated ID cols resulting from bind_cols operation
excluded_vars = c("ID1", "ID2")
stations.ext  = dplyr::select(stations.ext, -one_of(excluded_vars))

# renaming to stations.sf
stations.sf = stations.ext


# Injecting the ref of the closest px into the station locations
closest_px <- list()
for (st in seq_len(nrow(stations.sf))) {
  closest_px[[st]] <- grid.sf[which.min(
    st_distance(grid.sf, sf::st_transform(stations.sf[st,], sf::st_crs(grid.sf)))),]
}
closest_px = do.call(rbind.data.frame, closest_px)
stations.sf = stations.sf %>%
  dplyr::bind_cols(data.frame(closest_px$px)) %>%
  dplyr::rename(closest_px = closest_px.px )

# creating the stations static features dataset
stations.df = stations.sf

# adding lat/lon data from stations.sf to stations.df
stations.df = stations.df %>%
  dplyr::left_join(
    (data.frame(st_coordinates(st_transform(stations.sf, 3812))) %>%
        dplyr::bind_cols(stations.sf[c("sid")]) %>%
        dplyr::select(-geometry)
    ),
    by = "sid"
  )

# converting to dataframe
sf::st_geometry(stations.df) = NULL


# station points geography object
stations.sf = stations.sf %>%
  dplyr::select(c(sid, poste, network_name, closest_px))


#####
## SAVING the stations OBJECTS TO PACKAGE DATA
# doc : http://r-pkgs.had.co.nz/data.html

# saving in ./data/*.rda
devtools::use_data(stations.sf, stations.df, internal = FALSE, overwrite = TRUE)

#####
## RMI INCA 1 MONTH HISTORICAL DATA HOURLY

# # load all the hourly datasets
# inca.hourly.file.list = list.files(path = "./data-raw/extdata/INCA_BE_H/", pattern = ".Rdata")
# # extract only 1 month because to heavy to load all at once
# inca.hourly.1month =  lapply(inca.hourly.file.list[(length(inca.hourly.file.list) - 24)], function(x) {
#   load(file = paste0("./data-raw/extdata/INCA_BE_H/",x))
#   get(ls()[ls() != "filename"])
# })
# # names(inca.hourly.1month) <- inca.hourly.file.list[(length(inca.hourly.file.list) - 24)]
# inca.hourly.1month = data.frame(inca.hourly.1month)
# colnames(inca.hourly.1month) = c("hour", "date", "px", "tsahp1")
# # make it spatial to only keep Wallonia
# inca.hourly.1month = inca.hourly.1month %>%
#   dplyr::filter(px %in% incaGrid$px) %>%
#   # convert date and hour to posixct
#   dplyr::mutate(mtime = paste(date, as.character(hour), sep = " ")) %>%
#   dplyr::mutate_at(.vars = "mtime", .funs = as.POSIXct, format = "%Y%m%d %H") %>%
#   dplyr::select(-one_of(c("hour", "date")))


#####
## AGROMET STATIONS HISTORICAL DATA 2015-2018


# # Read data from the API exported json file downloaded from PAMESEB FTP & create a dataframe
# records = jsonlite::fromJSON(
#     "./data-raw/extdata/AGROMET/twoYears.json") # available on AGROMET FTP
# records.data = records$results
# records.meta = records$references$stations
# records.l <- list(metadata = records.meta, data = records.data)
# records.data <- typeData(records.l, "cleandata")
# # Filtering records to keep only the useful ones (removing non relevant stations)
# records.data = records.data %>%
#   filter(network_name == "pameseb") %>%
#   filter(type_name != "Sencrop") %>%
#   filter(!is.na(to)) %>%
#   filter(state == "Ok") %>%
#   filter(!is.na(tsa)) %>%
#   filter(!is.na(ens))
# # saving as an object
#
#
# #####
# ## AGROMET STATIONS HISTORICAL DATA 1 month corresponding to inca.hourly.1month
#
# # filtering according to what is present in inca.hourly.1month
# records.hourly.1month = records.data %>%
#   dplyr::filter(mtime %in% inca.hourly.1month$mtime)
#








# # dynamic records at stations
# stations.dyn = records.hourly.1month
# stations.dyn = stations.dyn %>%
#   dplyr::select(c(sid, mtime, tsa, ens))
# # devtools::use_data(stations.dyn, overwrite = TRUE)
#
# # dynamic records at grid points
# grid.dyn = inca.hourly.1month
# colnames(grid.dyn) = c("px", "tsa_hp1", "mtime")
# # devtools::use_data(grid.dyn, overwrite = TRUE)
#
# # adding the px of closest point of grid to each stations of stations.sf
# closest_px <- list()
# for (st in seq_len(nrow(stations.sf))) {
#   closest_px[[st]] <- grid.sf[which.min(
#     st_distance(grid.sf, sf::st_transform(stations.sf[st,], sf::st_crs(grid.sf)))),]
# }
# closest_px = do.call(rbind.data.frame, closest_px)
# stations.sf = stations.sf %>%
#   dplyr::bind_cols(data.frame(closest_px$px)) %>%
#   dplyr::rename(px = closest_px.px )
# # devtools::use_data(stations.sf, overwrite = TRUE)
#
# # adding lat/lon data from stations.sf to stations.df
# stations.df = stations.df %>%
#   dplyr::left_join(
#     (data.frame(st_coordinates(st_transform(stations.sf, 3812))) %>%
#         dplyr::bind_cols(stations.sf["sid"]) %>%
#         dplyr::select(-geometry)
#     ),
#     by = "sid"
#   )
# sf::st_geometry(stations.df) = NULL



#+ ---------------------------------
#' ## Terms of service
#' The present script is available under the [GNU-GPL V3](https://www.gnu.org/licenses/gpl-3.0.en.html) license and comes with ABSOLUTELY NO WARRANTY.
#'
#' Copyright : Thomas Goossens - hello.pokyah@gmail.com 2018.
#'
#' *(This document was generated using [R software](https://www.r-project.org/) with the [knitr library](https://deanattali.com/2015/03/24/knitrs-best-hidden-gem-spin/))*.
#+ TOS,echo=TRUE,warning=FALSE,message=FALSE,error=FALSE