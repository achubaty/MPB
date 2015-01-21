## Libaries
library(sp)
library(raster)
library(RCurl)
library(rgdal)
library(rgeos)
library(snow)

## Set options
maps.dir <- "M:/data/"
#maps.dir <- "/mnt/A105254/shared/data/"
#maps.dir = "~/Documents/Data/maps"
if(!file.exists(maps.dir)) stop("maps dir does not exist.")

scenarios = c("rcp26", "rcp45", "rcp85")
years = as.character(2006L:2100L)

download = FALSE
num.cpus = 4
rasterOptions(maxmemory=1e9, chunksize=1e8)

## Study Region (process the entire conutry minus the territories)
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
study.region = c("British Columbia", "Alberta", "Saskatchewan", "Manitoba",
                 "Ontario", "Québec", "New Brunswick", "Nova Scotia",
                 "Newfoundland", "Prince Edward Island")

# provicial boundaries
load(file.path(maps.dir, "CAN_adm1.RData"))
canada1 = gadm
canada1.boreal = spTransform(canada1, crs.boreal)
SR.boreal = canada1.boreal[na.omit(match(study.region, canada1.boreal$NAME_1)),]
rm(gadm, canada1)

# study area (correct for non-adjacent boundaries)
SR.boreal.union.buff = gBuffer(SR.boreal, width=1e-5)
boreal.SR = gIntersection(SR.boreal.union.buff, boreal.can, byid=TRUE)
rm(SR.boreal.union.buff)

## Data directories
tmpdir = tmpDir()

if (download) {
  ## Get files of size zero
  size.zero <- function(path=".", size=0) {
    files = dir(path, full.names=TRUE, recursive=TRUE)
    return(files[file.info(files)$size==size])
  }
  
  eol = ifelse(Sys.info()[["sysname"]]=="Windows", "\r\n", "\n")
  
  ## Define the ftp address (NOTE this address is temporary)
  ftpsite <- "ftp://ftp.nrcan.gc.ca/pub/outgoing/CANESM2_HISTORICAL/"
  
  for (i in scenarios) { 
    for (j in years) {
      ftppath = paste0(ftpsite, i, "/asciigrids/", j, "/")
      locpath = file.path(maps.dir, "climate", "CANESM2_HISTORICAL", i, "asciigrids", j)
      if(!file.exists(locpath)) dir.create(locpath, recursive=TRUE)
      
      files = unlist(strsplit(getURL(ftppath, dirlistonly=TRUE), split=eol))
      
      for (k in files) {
        try(download.file(paste0(ftppath, k), file.path(locpath, k)))
      }
    }
  }
}

