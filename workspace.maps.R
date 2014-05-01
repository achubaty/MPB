###
### SETUP R WORKSPACE TO WORK WITH MPB MAP DATA
###

# data packages
library(data.table)

# GIS and spatial packages
library(maptools)
library(maps)
library(mapdata)
library(plotKML)
library(raster)
library(rgdal)
library(rgeos)
library(rts)
library(shapefiles)

# HPC packages
library(snowfall)

# plotting packages
library(RColorBrewer)

if (Sys.info()[["sysname"]]=="Darwin") {
  maps.dir = "~/Documents/data/maps"
  work.dir = "~/Documents/GitHub/MPB"
} else if (Sys.info()[["sysname"]]=="Linux") {
  if (pmatch("W-VIC", Sys.info()[["nodename"]], nomatch=0)) {
    maps.dir = "~/data/maps"
    work.dir = "~/GitHub/MPB"
  } else {
    maps.dir = "~/Documents/data/maps"
    work.dir = "~/Documents/GitHub/MPB"
  }
} else if (Sys.info()[["sysname"]]=="Windows") {
  maps.dir = "~/data/maps"
  work.dir = "~/GitHub/MPB"
} else {
  print("Which operating system are you using?")
}
setwd(work.dir)
rdata.path = file.path(maps.dir, "MPB", "Rmaps")

getOGR <- function(layer, dir) {
  orig.dir = getwd()
  setwd(dir)
  out = readOGR(dsn=".", layer=layer)
  setwd(orig.dir)
  return(out)
}

loadObjects <- function(objects, path) {
  lapply(objects, function(x) load(file=paste(rdata.path, "/", x, ".rdata", sep=""), env=globalenv()))
}

saveObjects <- function(objects, path) {
  lapply(objects, function(x) save(list=x, file=paste(path, "/", x, ".rdata", sep="")))
}
