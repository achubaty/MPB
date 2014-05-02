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
  ab.poly.boreal = sfClusterApplyLB(ab.poly, spTransform, crs.boreal)
  names(ab.poly.boreal) = names(ab.poly)
  saveObjects("ab.poly.boreal", rdata.path)

  bc.poly.boreal = sfClusterApplyLB(bc.poly, spTransform, crs.boreal)
  names(bc.poly.boreal) = names(bc.poly)
  saveObjects("bc.poly.boreal", rdata.path)

  rm(ab.poly.boreal, bc.poly.boreal)
sfStop()


# manual garbage collection to free recently unallocated memory
for (i in 1:10) gc()
