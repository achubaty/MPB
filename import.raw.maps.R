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
  out = readOGR(dsn=".", layer=layer)
  setwd(orig.dir)
  return(out)
}

################################################################################
### PROCESS AB AND BC MAPS (POINTS & POLYGONS)
sfInit(cpus=num.cpus, parallel=TRUE)
  sfLibrary(rgdal)
  sfLibrary(sp)
    
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

  ### US maps
  us.poly.files = dir(path=file.path(maps.dir, "MPB", "US"), pattern="us_mpb")
  us.poly.dir.shp = unique(sapply(strsplit(us.poly.files, "\\."), function(x) x[[1]]))
  us.poly = sfClusterApplyLB(us.poly.dir.shp, fun=getOGR, dir=file.path(maps.dir, "MPB", "US"))
  names(us.poly) = sapply(strsplit(us.poly.dir.shp,"mpb"), function(x) x[[2]])

  us.poly.pre2006 = us.poly[["1997to2005"]] # no 2005?
  us.poly = us.poly[-1] # drop "1997to2005" from the list
  
  years.post = names(us.poly)
  years.pre = unique(us.poly.pre2006$YEAR)
  for (year in years.pre) {
    ids = which(us.poly.pre2006$YEAR == year)
    us.poly = append(us.poly.pre2006[ids,], us.poly)
  }
  names(us.poly) = c(rev(years.pre), years.post)
  
sfStop()

### LOAD OTHER MAPS, PROVINCE OUTLINES, COUNTY OUTLINES, BOREAL FOREST
boreal <- getOGR("NABoreal", file.path(maps.dir, "boreal"))
crs.boreal = CRS(proj4string(boreal))

load(file.path(maps.dir, "CAN_adm1.RData"))
canada1 = gadm
canada1.boreal = spTransform(canada1, crs.boreal)
west = canada1.boreal[na.omit(match(c("Alberta", "British Columbia","Saskatchewan"), canada1.boreal$NAME_1)),]
rm(gadm, canada1, canada1.boreal)

load(file.path(maps.dir, "CAN_adm2.RData"))
canada2 = gadm
canada2.boreal = spTransform(canada2, crs.boreal)
canada2.boreal.dt = data.table(data.frame(canada2.boreal))
setkey(canada2.boreal.dt, "NAME_1")
subset = match(canada2.boreal.dt[c("Alberta", "British Columbia", "Saskatchewan")]$PID, canada2.boreal@data$PID)
west.county = canada2.boreal[subset,]
rm(gadm, canada2, canada2.boreal, canada2.boreal.dt, subset)

# extent obtained using `locator()`
ext = extent(x=-1027658, xmax=320751.9, ymin=5108872, ymax=6163350)
west.empty = raster(ext)
res(west.empty) <- 100
west.r = rasterize(west, west.empty)

### REPROJECT AB, BC, US SO THEY ARE BOTH IN THE `boreal` PROJECTION
sfInit(cpus=num.cpus, parallel=TRUE)
  sfLibrary(rgdal)
  sfExport("crs.boreal")
  ab.bor = sfClusterApplyLB(ab, spTransform, crs.boreal)
  bc.bor = sfClusterApplyLB(bc, spTransform, crs.boreal)
sfStop()

sfInit(cpus=num.cpus, parallel=TRUE)
  sfLibrary(rgdal)
  sfExport("crs.boreal")
  ab.poly.bor = sfClusterApplyLB(ab.poly, spTransform, crs.boreal)
  bc.poly.bor = sfClusterApplyLB(bc.poly, spTransform, crs.boreal)
  us.poly.bor = sfClusterApplyLB(us.poly, spTransform, crs.boreal)
sfStop()

# rename them to simpler names
rm(ab, ab.poly, bc, bc.poly, us.poly, us.poly.pre2006)
ab = ab.bor
ab.poly = ab.poly.bor
bc = bc.bor
bc.poly = bc.poly.bor
us.poly = us.poly.bor
names(ab.bor) = sapply(strsplit(ab.dir.shp,"_"),function(x) x[[3]])
names(bc.bor) = sapply(strsplit(bc.dir.shp,"_"),function(x) x[[3]])
names(ab.poly.bor) = sapply(strsplit(ab.poly.dir.shp,"_"),function(x) x[[3]])
names(bc.poly.bor) = sapply(strsplit(bc.poly.dir.shp,"_"),function(x) x[[3]])
names(us.poly.bor) = c(rev(years.pre), years.post)
rm(ab.bor, ab.poly.bor, bc.bor, bc.poly.bor, us.poly.bor, years.pre, years.post)

# save these new map objects for later use
path = file.path(maps.dir, "MPB", "Rmaps")
objects2save = c("ab", "ab.poly", "bc", "bc.poly", "boreal", "us.poly")
lapply(objects2save, function(x) save(list=x, file=paste(path, "/", x, ".rdata", sep="")))


### REPROJECT WEST SO IT'S IN THE `boreal` PROJECTION
sfInit(cpus=num.cpus, parallel=TRUE)
  sfLibrary(rgdal)
  sfExport("crs.boreal")
  west.bor = spTransform(west, crs.boreal)
  west.county.bor = spTransform(west.county, crs.boreal)
sfStop()

# WARNING: this eats up RAM like crazy! use fewer cpus
# (RAM usage fluctuates, but I've seen it use up to 6.5 GB per cpu)
beginCluster(n=ceiling(num.cpus/4))
  west.r.bor = projectRaster(west.r, res=, crs=crs.boreal)
endCluster()

# rename them to simpler names
rm(west, west.county, west.r)
west.r = west.r.bor
west = west.bor
west.county = west.county.bor
rm(west.bor, west.county.bor, west.r.bor)

### Save these new map objects for later use
path = file.path(maps.dir, "MPB", "Rmaps")
objects2save = c("west", "west.county", "west.r")
lapply(objects2save, function(x) save(list=x, file=paste(path, "/", x, ".rdata", sep="")))
