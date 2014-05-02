###
### LOAD WORKSPACE SETTINGS
###   - make sure `num.cpus` is set
if (!exists("WORKSPACE")) source("workspace.maps.R")
if(!exists("crs.boreal")) {
  loadObjects("boreal", rdata.path)
  crs.boreal = CRS(proj4string(boreal))
  rm(boreal)
}

### REPROJECT AB, BC SO THEY ARE BOTH IN THE `boreal` PROJECTION
sfInit(cpus=num.cpus, parallel=TRUE)
  sfLibrary(rgdal)
  
  ab.pnts.boreal = sfClusterApplyLB(ab.pnts, spTransform, crs.boreal)
  bc.pnts.boreal = sfClusterApplyLB(bc.pnts, spTransform, crs.boreal)

  names(ab.pnts.boreal) = names(ab.pnts)
  names(bc.pnts.boreal) = names(bc.pnts)

  # save these new map objects for later use
  saveObjects(c("ab.pnts.boreal", "bc.pnts.boreal"), rdata.path)

  # clean up workspace
  rm(ab.pnts.boreal, bc.pnts.boreal)
sfStop()
