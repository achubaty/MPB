## Libaries
library(sp)
library(raster)
library(RCurl)
library(rgdal)
library(rgeos)
library(snow)

## Set options
maps.dir = "~/Documents/Data/maps"
if(!file.exists(maps.dir)) stop("maps dir does not exist.")

download = FALSE
num.cpus = 4
rasterOptions(maxmemory=1e9, chunksize=1e8)

## Study Region (process the entire country minus the territories)
getOGR <- function(layer, dir) {
  orig.dir = getwd()
  setwd(dir)
  out = readOGR(dsn=".", layer=layer)
  setwd(orig.dir)
  return(out)
}

boreal = getOGR("NABoreal", file.path(maps.dir, "boreal"))
boreal.can = boreal[boreal$COUNTRY=="CANADA",]
crs.boreal = CRS(proj4string(boreal))
study.region = c("British Columbia", "Alberta")
#study.region = c("British Columbia", "Alberta", "Saskatchewan", "Manitoba",
#                 "Ontario", "Québec", "New Brunswick", "Nova Scotia",
#                 "Newfoundland", "Prince Edward Island")
rm(boreal, boreal.can)

# provicial boundaries
load(file.path(maps.dir, "CAN_adm1.RData"))
canada1 = gadm
canada1.boreal = spTransform(canada1, crs.boreal)
SR.boreal = canada1.boreal[na.omit(match(study.region, canada1.boreal$NAME_1)),]
rm(gadm, canada1)

# study area (correct for non-adjacent boundaries)
SR.boreal.union.buff = gBuffer(SR.boreal, width=1e-5)
boreal.SR = intersect(SR.boreal.union.buff, boreal.can)
rm(SR.boreal.union.buff)

## Data directories
dem50k = file.path(maps.dir, "cded","50k_dem")
dem250k = file.path(maps.dir, "cded","250k_dem")
tmpdir = tmpDir()
tmpdir50k = file.path(tmpdir, "50k_dem")
tmpdir250k = file.path(tmpdir, "250k_dem")

## Fetch elevation data from internet
if (download) {
  if (os=="windows") {
    source("~/GitHub/McIntire-lab/code/data-sources/cded-download.R")
  } else {
    source("~/Documents/GitHub/McIntire-lab/code/data-sources/cded-download.R")
  }
}

##------------------------------------------------------------------------------
##                                   250M_DEM
##------------------------------------------------------------------------------
## Unzip data
invisible(sapply(dir(file.path(dem250k), recursive=TRUE, pattern="[.]zip$",
                     full.names=TRUE), unzip, exdir=tmpdir250k))

## Note: dem(e) for East and dem(w) for West
files <- dir(tmpdir250k, pattern="[.]dem$", full.names=TRUE)
SR <- c("072", "073", "074", "082", "083", "084", "091", "092", "093", "094",
        "101", "102", "103", "104", "113", "114") # manually: BC, AB
files.SR <- unlist(lapply(SR, function(x) { grep(x, files, value=TRUE) }))

dem.SR <- do.call(merge, lapply(files.SR, raster))
#dem.SR <- raster(file.path(maps.dir, "cded", "dem_SR_250k.grd"))

#dem.all <- do.call(merge, lapply(files, raster))
#dem.all <- raster(file.path(maps.dir, "cded", "dem_all_250k.grd"))

#beginCluster(num.cpus)
dem.SR.boreal <- projectRaster(from=dem.SR, crs=crs.boreal)
#dem.all.boreal <- projectRaster(from=dem.all, crs=crs.boreal)
#endCluster()

elev.SR.boreal <- clip.raster(dem.SR.boreal, boreal.SR)
#elev.boreal <- clip.raster(dem.all.boreal, boreal.SR)

writeRaster(elev.SR.boreal, file.path(maps.dir, "cded", "elevation_SR_250k.tif"))
#writeRaster(elev.boreal, file.path(maps.dir, "cded", "elevation_250k.tif"))

## Clean directory: keep only zip files
lapply(grep(dir(file.path(dem250k), full.names=TRUE),
            pattern="[.]zip$", value=TRUE, invert=TRUE), file.remove)

##------------------------------------------------------------------------------
##                                   50M_DEM
##------------------------------------------------------------------------------
## Unzip data
invisible(sapply(dir(file.path(dem50k), pattern="[.]zip$",
                     full.names=TRUE), unzip, exdir=tmpdir250k))

## Note: dem(e) for East and dem(w) for West
dem <- lapply(dir(tmpdir250k, pattern="[.]dem$", full.names=TRUE), raster)
dem.all <- do.call(merge, dem)

#beginCluster(num.cpus)
dem.all.boreal <- projectRaster(from=dem.all, crs=crs.boreal, method="bilinear")
#endCluster()

elev.boreal <- clip.raster(dem.all.boreal, boreal.SR)

writeRaster(elev.boreal, file.path(maps.dir, "cded", "elevation_50k.tif"))

## Clean directory: keep only zip files
lapply(grep(dir(file.path(dem50k), full.names=TRUE),
            pattern="[.]zip$", value=TRUE, invert=TRUE), file.remove)

##------------------------------------------------------------------------------
##
##------------------------------------------------------------------------------
srRt_100m <- disaggregate(srRt, fact=100)

dem_250k_100m <- projectRaster(from=dem_250k, to=srRt_100m)
dem_50k_100m <- projectRaster(from=dem_50k, to=srRt_100m)

sd_dem_250k <- aggregate(dem_250k_100m, fact=100, fun=sd)
sd_dem_50k <- aggregate(dem_50k_100m, fact=100, fun=sd)
