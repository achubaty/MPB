## Set options
._CFS_. <- grepl("W-VIC", Sys.info()[["nodename"]])
._OS_. <- Sys.info()[["sysname"]]

maps.dir <- if (._OS_. == "Windows") {
  "//W-VIC-A105388/shared/data"
} else if (._OS_. == "Darwin") {
  "~/Documents/shared"
} else if (._OS_. == "Linux") {
  ifelse(isTRUE(._CFS_.), "/mnt/A105388/shared/data", "~/Documents/Data/shared")
}
stopifnot(dir.exists(maps.dir))

download50k = FALSE
download250k = FALSE

reprocess50k = FALSE
reprocess250k = FALSE

num.cpus = parallel::detectCores() / 2

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
f <- file.path(maps.dir, "cded", "dem_all_250k.RData")
fr <- file.path(maps.dir, "cded", "dem_all_250k.grd")
if (file.exists(f)) {
  load(f)
} else if (file.exists(fr)) {
  dem_all_250k <- raster(fr)
} else {
  stop("Unable to load file. Are you sure you have it in your maps.dir?")
}

##------------------------------------------------------------------------------
## from Jean; incomplete!
##------------------------------------------------------------------------------
srRt_100m <- disaggregate(srRt, fact = 100)

dem_250k_100m <- projectRaster(from = dem_250k, to = srRt_100m)
dem_50k_100m <- projectRaster(from = dem_50k, to = srRt_100m)

sd_dem_250k <- aggregate(dem_250k_100m, fact = 100, fun = sd)
sd_dem_50k <- aggregate(dem_50k_100m, fact = 100, fun = sd)
