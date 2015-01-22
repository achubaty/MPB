## Set options
os = tolower(Sys.info()[["sysname"]])
maps.dir = if (os=="windows") {
  "//W-VIC-A105254/shared/data"
} else if (os=="Darwin") {
  "~/Documents/Data/maps"
} else if (os=="linux") {
  "/mnt/A105254/shared/data"
}
if(!file.exists(maps.dir)) stop("maps dir does not exist.")

download50k = FALSE
download250k = FALSE

reprocess50k = FALSE
reprocess250k = FALSE

num.cpus = 4

## Libaries
library(sp)
library(raster); rasterOptions(maxmemory=2e9, chunksize=5e7)
library(rgdal)
library(rgeos)
library(snow)

if (download50k || download250k) library(RCurl)

## Helper functions
getOGR <- function(layer, dir) {
  orig.dir = getwd()
  setwd(dir)
  out = readOGR(dsn=".", layer=layer)
  setwd(orig.dir)
  return(out)
}

## Define Study Region
boreal = getOGR("NABoreal", file.path(maps.dir, "boreal"))
boreal.can = boreal[boreal$COUNTRY=="CANADA",]
crs.boreal = CRS(proj4string(boreal))
study.region = c("British Columbia", "Alberta", "Saskatchewan")
rm(boreal)

# provicial boundaries
load(file.path(maps.dir, "CAN_adm", "CAN_adm1.RData"))
canada1 = gadm
canada1.boreal = spTransform(canada1, crs.boreal)
SR.boreal = canada1.boreal[na.omit(match(study.region, canada1.boreal$NAME_1)),]
rm(canada1, gadm)

# study area (correct for non-adjacent boundaries)
SR.boreal.union.buff = gBuffer(SR.boreal, width=1e-5)
#boreal.SR = intersect(SR.boreal.union.buff, boreal.can)
boreal.SR = gIntersection(SR.boreal.union.buff, boreal.can, byid=TRUE)
save(boreal.can, file=file.path(maps.dir, "boreal", "Rdata", "boreal.can.RData"))
save(boreal.SR, file=file.path(maps.dir, "boreal", "Rdata", "boreal.SR.RData"))
save(SR.boreal.union.buff, file=file.path(maps.dir, "boreal", "Rdata", "SR.boreal.union.buff.RData"))
rm(boreal.can, SR.boreal.union.buff)

## Data directories
dem50k = file.path(maps.dir, "cded", "50k_dem")
dem250k = file.path(maps.dir, "cded", "250k_dem")
tmpdir = tmpDir()
tmpdir50k = file.path(tmpdir, "50k_dem")
tmpdir250k = file.path(tmpdir, "250k_dem")

## Fetch elevation data from internet
if (download50k) {
  if (os=="windows") {
    system("cd ~/GitHub/McIntire-lab && git pull", intern=TRUE, wait=TRUE)
    source("~/GitHub/McIntire-lab/code/data-sources/cded-download-50k.R")
  } else {
    system("cd ~/Documents/GitHub/McIntire-lab && git pull", intern=TRUE, wait=TRUE)
    source("~/Documents/GitHub/McIntire-lab/code/data-sources/cded-download-50k.R")
  }
}

if (download250k) {
  if (os=="windows") {
    system("cd ~/GitHub/McIntire-lab && git pull", intern=TRUE, wait=TRUE)
    source("~/GitHub/McIntire-lab/code/data-sources/cded-download-250k.R")
  } else {
    system("cd ~/Documents/GitHub/McIntire-lab && git pull", intern=TRUE, wait=TRUE)
    source("~/Documents/GitHub/McIntire-lab/code/data-sources/cded-download-250k.R")
  }
}


## Reprocess each ToI and reproject for new study area
if (reprocess50k) {
  if (os=="windows") {
    system("cd ~/GitHub/McIntire-lab && git pull", intern=TRUE, wait=TRUE)
    source("~/GitHub/McIntire-lab/code/data-sources/cded-reprocess-50k.R")
  } else {
    system("cd ~/Documents/GitHub/McIntire-lab && git pull", intern=TRUE, wait=TRUE)
    source("~/Documents/GitHub/McIntire-lab/code/data-sources/cded-reprocess-50k.R")
  }
}

if (reprocess250k) {
  if (os=="windows") {
    system("cd ~/GitHub/McIntire-lab && git pull", intern=TRUE, wait=TRUE)
    source("~/GitHub/McIntire-lab/code/data-sources/cded-reprocess-250k.R")
  } else {
    system("cd ~/Documents/GitHub/McIntire-lab && git pull", intern=TRUE, wait=TRUE)
    source("~/Documents/GitHub/McIntire-lab/code/data-sources/cded-reprocess-250k.R")
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
SR <- c("062", "063", "064", "072", "073", "074",
        "082", "083", "084", "091", "092", "093", "094",
        "101", "102", "103", "104", "113", "114") # manually: BC, AB, SK
SR <- paste0("^", SR)
files.SR <- unlist(lapply(SR, function(x) {
  i = grep(x, basename(files))
  file.path(dirname(files[i]), basename(files[i]))
  }))

tif250k <- file.path(maps.dir, "cded", "dem_all_250k.tif")
#if (file.exists(tif250k)) {
#  dem.all <- raster(tif250k)
#} else {
#  dem.all <- do.call(merge, lapply(files, raster))
#  writeRaster(dem.all, filename=tif250k, overwrite=TRUE)
#}

tif250k.SR <- file.path(maps.dir, "cded", "dem_SR_250k.tif")
if (file.exists(tif250k.SR)) {
  dem.SR <- raster(tif250k.SR)
} else {
  dem.SR <- do.call(merge, lapply(files.SR, raster))
  writeRaster(dem.SR, filename=tif250k.SR, overwrite=TRUE)
}

beginCluster(num.cpus)
dem.SR.boreal <- projectRaster(from=dem.SR, crs=crs.boreal)
#dem.all.boreal <- projectRaster(from=dem.all, crs=crs.boreal)
endCluster()

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
                     full.names=TRUE), unzip, exdir=tmpdir50k))

## Note: dem(e) for East and dem(w) for West
dem <- lapply(dir(tmpdir50k, pattern="[.]dem$", full.names=TRUE), raster)
dem.all <- do.call(merge, dem)

beginCluster(num.cpus)
dem.all.boreal <- projectRaster(from=dem.all, crs=crs.boreal, method="bilinear")
endCluster()

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
