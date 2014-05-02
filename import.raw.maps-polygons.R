###
### LOAD WORKSPACE SETTINGS
###   - make sure `num.cpus` is set
if (!exists("WORKSPACE")) source("workspace.maps.R")

### AB maps
sfInit(cpus=num.cpus, parallel=TRUE)
  sfLibrary(rgdal)
  sfLibrary(sp)
    
  ab.poly.files = dir(path=file.path(maps.dir, "MPB", "ab_mpb"), pattern="poly")
  ab.poly.dir.shp = unique(sapply(strsplit(ab.poly.files, "\\."), function(x) x[[1]]))
  ab.poly = sfClusterApplyLB(ab.poly.dir.shp, fun=getOGR, dir=file.path(maps.dir, "MPB", "ab_mpb"))
  names(ab.poly) = substr(sapply(strsplit(ab.poly.dir.shp,"_"), function(x) x[[3]]), 1, 4)
  
  # save these new map objects for later use
  saveObjects("ab.poly", rdata.path)

  # clean up workspace
  rm(ab.poly, ab.poly.dir.shp, ab.poly.files)
sfStop()

### BC maps
sfInit(cpus=num.cpus, parallel=TRUE)
  sfLibrary(rgdal)
  sfLibrary(sp)
  
  bc.poly.files = dir(path=file.path(maps.dir, "MPB", "province_BC"), pattern="poly")
  bc.poly.dir.shp = unique(sapply(strsplit(bc.poly.files, "\\."), function(x) x[[1]]))
  bc.poly = sfClusterApplyLB(bc.poly.dir.shp, fun=getOGR, dir=file.path(maps.dir, "MPB", "province_BC"))
  names(bc.poly) = sapply(strsplit(bc.poly.dir.shp,"_"), function(x) x[[3]])
  
  # save these new map objects for later use
  saveObjects("bc.poly", rdata.path)
  
  # clean up workspace
  rm(bc.poly, bc.poly.files, bc.poly.dir.shp)
sfStop()

# manual garbage collection to free recently unallocated memory
for (i in 1:10) gc()
