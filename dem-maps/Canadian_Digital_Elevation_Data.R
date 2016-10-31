## Set options
CFS <- grepl("W-VIC", Sys.info()[["nodename"]])
OS <- Sys.info()[["sysname"]]

maps.dir <- if (OS == "Windows") {
  "//W-VIC-A105388/shared/data"
} else if (OS == "Darwin") {
  "~/Documents/shared"
} else if (OS == "Linux") {
  ifelse(isTRUE(CFS), "/mnt/A105388/shared/data", "~/Documents/shared")
}
if (!dir.exists(maps.dir)) stop("maps.dir does not exist.")

download50k = FALSE
download250k = FALSE

reprocess50k = FALSE
reprocess250k = FALSE

num.cpus = 4

## Libaries
library(devtools)
library(sp)
library(raster); rasterOptions(maxmemory = 2e9, chunksize = 5e7)
library(rgdal)
library(rgeos)
library(snow)

if (download50k || download250k) library(RCurl)

## Data directories
dem50k = file.path(maps.dir, "cded", "50k_dem")
dem250k = file.path(maps.dir, "cded", "250k_dem")
tmpdir = tmpDir()
tmpdir50k = file.path(tmpdir, "50k_dem")
tmpdir250k = file.path(tmpdir, "250k_dem")

## Fetch elevation data from internet
if (download50k) {
  source_url("https://raw.githubusercontent.com/eliotmcintire/McIntire-lab/master/code/data-sources/cded-download-50k.R")
}

if (download250k) {
  source_url("https://raw.githubusercontent.com/eliotmcintire/McIntire-lab/master/code/data-sources/cded-download-250k.R")
}

## Reprocess each ToI and reproject for new study area
##
## NOTE: be sure to set options, including study region definition within these files!
##
if (reprocess50k) {
  source_url("https://raw.githubusercontent.com/eliotmcintire/McIntire-lab/master/code/data-sources/cded-reprocess-50k.R")
}

if (reprocess250k) {
  source_url("https://raw.githubusercontent.com/eliotmcintire/McIntire-lab/master/code/data-sources/cded-reprocess-250k.R")
}

## Load previously saved DEM objects
load(file.path(maps.dir, "cded", "dem_SR_boreal_250k.RData"))

##------------------------------------------------------------------------------
## from Jean; incomplete!
##------------------------------------------------------------------------------
srRt_100m <- disaggregate(srRt, fact = 100)

dem_250k_100m <- projectRaster(from = dem_250k, to = srRt_100m)
dem_50k_100m <- projectRaster(from = dem_50k, to = srRt_100m)

sd_dem_250k <- aggregate(dem_250k_100m, fact = 100, fun = sd)
sd_dem_50k <- aggregate(dem_50k_100m, fact = 100, fun = sd)
