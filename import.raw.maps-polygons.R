###
### LOAD WORKSPACE SETTINGS
###   - make sure `num.cpus` is set
source("workspace.maps.R")

### PROCESS AB AND BC MAPS (POINTS & POLYGONS)
sfInit(cpus=num.cpus, parallel=TRUE)
  sfLibrary(rgdal)
  sfLibrary(sp)
    
  ### AB maps
  ab.poly.files = dir(path=file.path(maps.dir, "MPB", "ab_mpb"), pattern="poly")
  ab.poly.dir.shp = unique(sapply(strsplit(ab.poly.files, "\\."), function(x) x[[1]]))
  ab.poly = sfClusterApplyLB(ab.poly.dir.shp, fun=getOGR, dir=file.path(maps.dir, "MPB", "ab_mpb"))
  names(ab.poly) = substr(sapply(strsplit(ab.poly.dir.shp,"_"), function(x) x[[3]]), 1, 4)
  
  ### BC maps
  bc.poly.files = dir(path=file.path(maps.dir, "MPB", "province_BC"), pattern="poly")
  bc.poly.dir.shp = unique(sapply(strsplit(bc.poly.files, "\\."), function(x) x[[1]]))
  bc.poly = sfClusterApplyLB(bc.poly.dir.shp, fun=getOGR, dir=file.path(maps.dir, "MPB", "province_BC"))
  names(bc.poly) = sapply(strsplit(bc.poly.dir.shp,"_"), function(x) x[[3]])
  
  # save these new map objects for later use
  saveObjects(c("ab.poly", "bc.poly"), rdata.path)

  # clean up workspace
  rm(ab.poly.dir.shp, ab.poly.files, bc.poly.files, bc.poly.dir.shp)
sfStop()
