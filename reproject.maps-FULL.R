###
### LOAD WORKSPACE SETTINGS
###   - make sure `num.cpus` is set
source("workspace.maps.R")

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
objects2save = c("ab", "ab.poly", "bc", "bc.poly", "boreal", "us.poly")
lapply(objects2save, function(x) save(list=x, file=paste(rdata.path, "/", x, ".rdata", sep="")))


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
objects2save = c("west", "west.county", "west.r")
lapply(objects2save, function(x) save(list=x, file=paste(rdata.path, "/", x, ".rdata", sep="")))
