library(data.table)
library(raster)
library(rgdal)
library(snowfall)

num.cpus = 4

if (Sys.info()[["sysname"]]=="Darwin") {
  maps.dir = "~/Documents/data/maps"
  work.dir = "~/Documents/GitHub/MPB"
} else if (Sys.info()[["sysname"]]=="Linux") {
  maps.dir = "~/Documents/data/maps"
  work.dir = "~/Documents/GitHub/MPB"
} else if (Sys.info()[["sysname"]]=="Windows") {
  maps.dir = "~/data/maps"
  work.dir = "~/GitHub/MPB"
} else {
  print("Which operating system are you using?")
}
setwd(work.dir)

getOGR <- function(layer, dir) {
  orig.dir = getwd()
  setwd(dir)
  readOGR(dsn=".", layer=layer)
  setwd(orig.dir)
}

################################################################################
### PROCESS AB AND BC MAPS
sfInit(cpus=num.cpus, parallel=TRUE)
  sfLibrary(sp)
  sfLibrary(rgdal)
  
  ### AB maps
  ab.files = dir(path=file.path(maps.dir, "MPB", "ab_mpb"), pattern="spot")
  ab.dir.shp = unique(sapply(strsplit(ab.files, "\\."), function(x) x[[1]]))
  ab = sfClusterApplyLB(ab.dir.shp, fun=getOGR, dir=file.path(maps.dir, "MPB", "ab_mpb"))
  names(ab) = sapply(strsplit(ab.dir.shp,"_"), function(x) x[[3]])
  
  ab.poly.files = dir(path=file.path(maps.dir, "MPB", "ab_mpb"), pattern="poly")
  ab.poly.dir.shp = unique(sapply(strsplit(ab.poly.files, "\\."), function(x) x[[1]]))
  ab.poly = sfClusterApplyLB(ab.poly.dir.shp, fun=getOGR, dir=file.path(maps.dir, "MPB", "ab_mpb"))
  names(ab.poly) = sapply(strsplit(ab.poly.dir.shp,"_"), function(x) x[[3]])
  
  ### BC maps
  bc.files = dir(path=file.path(maps.dir, "MPB", "province_BC"), pattern="spot")
  bc.dir.shp = unique(sapply(strsplit(bc.files, "\\."), function(x) x[[1]]))
  bc = sfClusterApplyLB(bc.dir.shp, fun=getOGR, dir=file.path(maps.dir, "MPB", "province_BC"))
  names(bc) = sapply(strsplit(bc.dir.shp,"_"), function(x) x[[3]])
  
  bc.poly.files = dir(path=file.path(maps.dir, "MPB", "province_BC"), pattern="poly")
  bc.poly.dir.shp = unique(sapply(strsplit(bc.poly.files, "\\."), function(x) x[[1]]))
  bc.poly = sfClusterApplyLB(bc.poly.dir.shp, fun=getOGR, dir=file.path(maps.dir, "MPB", "province_BC"))
  names(bc.poly) = sapply(strsplit(bc.poly.dir.shp,"_"), function(x) x[[3]])

sfStop()

################################################################################
### LOAD OTHER MAPS, PROVINCE OUTLINES, COUNTY OUTLINES, BOREAL FOREST
boreal <- getOGR("NABoreal", file.path(maps.dir, "boreal"))

load(file.path(maps.dir, "CAN_adm1.RData"))
canada1 = gadm
canada1.boreal = spTransform(canada1, CRS(proj4string(boreal)))
west = canada1.boreal[na.omit(match(c("Alberta", "British Columbia","Saskatchewan"), canada1.boreal$NAME_1)),]
rm(gadm, canada1, canada1.boreal)

load(file.path(maps.dir, "CAN_adm2.RData"))
canada2 = gadm
canada2.boreal = spTransform(canada2, CRS(proj4string(boreal)))
canada2.boreal.dt = data.table(data.frame(canada2.boreal))
setkey(canada2.boreal.dt, "NAME_1")
subset = match(canada2.boreal.dt[c("Alberta", "British Columbia", "Saskatchewan")]$PID, canada2.boreal@data$PID)
west.county = canada2.boreal[subset,]
rm(gadm, canada2, canada2.boreal, canada2.boreal.dt, subset)
#plot(west.county)

ext = extent(x=-1027658, xmax=320751.9, ymin=5108872, ymax=6163350)
west.empty = raster(ext)
res(west.empty) <- 1000
west.r = rasterize(west, west.empty)

################################################################################
### REPROJECT BC AND AB SO THEY ARE BOTH IN THE SAME PROJECTION
###   (USE THE ONE FOR THE BOREAL DATASET)
crs.boreal = CRS(proj4string(boreal))

sfInit(cpus=num.cpus, parallel=TRUE)
  sfExport("crs.boreal")
  ab.bor = sfClusterApplyLB(ab, spTransform, crs.boreal)
  bc.bor = sfClusterApplyLB(bc, spTransform, crs.boreal)
sfStop()

sfInit(cpus=num.cpus, parallel=TRUE)
  bc.poly.bor = sfClusterApplyLB(bc.poly, spTransform, crs.boreal)
sfStop()

sfInit(cpus=num.cpus, parallel=TRUE)
  ab.poly.bor = sfClusterApplyLB(ab.poly, spTransform, crs.boreal)
  west.bor = spTransform(west, crs.boreal)
  west.county.bor = spTransform(west.county, crs.boreal)
  west.r.bor = projectRaster(west.r,crs=crs.boreal)
sfStop()

rm(ab, ab.poly, bc, bc.poly, west, west.county, west.r)
names(bc.poly.bor) = sapply(strsplit(bc.poly.dir.shp,"_"),function(x) x[[3]])
names(ab.poly.bor) = sapply(strsplit(ab.poly.dir.shp,"_"),function(x) x[[3]])
names(bc.bor) = sapply(strsplit(bc.dir.shp,"_"),function(x) x[[3]])
names(ab.bor) = sapply(strsplit(ab.dir.shp,"_"),function(x) x[[3]])

### Rename them to simpler names
bc = bc.bor
bc.poly=bc.poly.bor
ab = ab.bor
ab.poly=ab.poly.bor
west.r = west.r.bor
west = west.bor
west.county = west.county.bor
rm(bc.poly.bor,ab.poly.bor,bc.bor, ab.bor,west.bor,west.county.bor,west.r.bor)

path = file.path(maps.dir, "MPB", "Rmaps")
objects2save = c("bc", "bc.poly", "ab", "ab.poly", "west", "west.county", "west.r")
lapply(objects2save, function(x) save(list=x, file=paste(path, "/", "mpb.", x, ".rdata", sep="")))

