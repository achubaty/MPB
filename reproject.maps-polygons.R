###
### LOAD WORKSPACE SETTINGS
###   - make sure `num.cpus` is set
if (!exists("WORKSPACE")) source("workspace.maps.R")
if(!exists("crs.boreal")) {
  loadObjects("boreal", rdata.path)
  crs.boreal = CRS(proj4string(boreal))
  rm(boreal)
}

### REPROJECT AB AND BC SO THEY ARE BOTH IN THE `boreal` PROJECTION
sfInit(cpus=num.cpus, parallel=TRUE)
  sfLibrary(rgdal)
  sfExport("ab.poly", "crs.boreal")
  ab.poly.boreal = sfClusterApplyLB(ab.poly, spTransform, crs.boreal)
  names(ab.poly.boreal) = names(ab.pnts)
  saveObjects("ab.poly.boreal", rdata.path)
  rm(ab.poly.boreal)
sfStop()

sfInit(cpus=num.cpus, parallel=TRUE)
  sfLibrary(rgdal)
  sfExport("bc.poly", "crs.boreal")
  bc.poly.boreal = sfClusterApplyLB(bc.poly, spTransform, crs.boreal)
  names(bc.poly.boreal) = names(bc.pnts)
  saveObjects("bc.poly.boreal", rdata.path)
  rm(bc.poly.boreal)
sfStop()
