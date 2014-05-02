###
### LOAD WORKSPACE SETTINGS
###   - cpus is set to `guesstimate` instead of `num.cpus`
###   - make sure `ext.maps` is set
###   - make sure `res.maps` is set
if (!exists("WORKSPACE")) source("workspace.maps.R")
if(!exists("crs.boreal")) {
  loadObjects("boreal", rdata.path)
  crs.boreal = CRS(proj4string(boreal))
  rm(boreal)
}

### we arbitrarily picked 1000 trees per 1ha
###   This NEEDS to be revisited.
change.res = function(x, y=west.boreal.raster, field=1000, fun="last", ...) {
  rasterize(x=x, y=y, field=field, fun=fun, ...)
}

sfInit(cpus=num.cpus, parallel=TRUE)
  sfLibrary(raster)
  sfExport("west.boreal.raster")
  
  ab.poly.boreal.raster.stack = stack(sfClusterApplyLB(ab.poly.boreal, change.res))
  names(ab.poly.boreal.raster.stack) = names(ab.poly.boreal)
  # several rasters have no values because they were in southern Alberta
  notNAs = which(sapply(1:nlayers(ab.poly.boreal.raster.stack),
                     function(x) unique(!is.na(which.min(ab.poly.boreal.raster.stack[[x]])))))
  ab.poly.boreal.raster = ab.poly.boreal.raster.stack[[notNAs]] # remove the NA layers
  names(ab.poly.boreal.raster.stack) = unlist(strsplit(names(ab.poly.boreal),"poly"))[notNAs]
  saveObjects("ab.poly.boreal.raster.stack", rdata.path)
    
  bc.poly.boreal.raster.stack = stack(sfClusterApplyLB(bc.poly.boreal, change.res))
  names(bc.poly.boreal.raster.stack) = names(bc.poly.boreal)
  saveObjects("bc.poly.boreal.raster.stack", rdata.path)
  
  rm(ab.poly.boreal.raster.stack, bc.poly.boreal.raster.stack, change.res, notNAs)
sfStop()
