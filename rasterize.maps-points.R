###
### LOAD WORKSPACE SETTINGS
###   - make sure `num.cpus` is set
###   - make sure `ext.maps` is set
###   - make sure `res.maps` is set
if (!exists("WORKSPACE")) source("workspace.maps.R")
if(!exists("crs.boreal")) {
  loadObjects("boreal", rdata.path)
  crs.boreal = CRS(proj4string(boreal))
  rm(boreal)
}

### rasterize ab and bc points maps
sfInit(cpus=num.cpus, parallel=TRUE)
  sfLibrary(raster)
  sfExport("west.boreal.raster")
  
  ab.pnts.boreal.raster.stack = stack(sfClusterApplyLB(ab.pnts.boreal, function(x) rasterize(x=x, y=west.boreal.raster, field=if(any(colnames(x@data)=="NUM_TREES")) x@data$NUM_TREES else x@data$num_trees, fun="sum")))
  names(ab.pnts.boreal.raster.stack) = names(ab.pnts.boreal)
  saveObjects("ab.pnts.boreal.raster.stack", rdata.path)
  rm(ab.pnts.boreal.raster.stack)

  bc.pnts.boreal.raster.stack = stack(sfClusterApplyLB(bc.pnts.boreal, function(x) rasterize(x=x, y=west.boreal.raster, field=x@data$NUM_TREES, fun="sum")))
  names(bc.pnts.boreal.raster.stack) = names(bc.pnts.boreal)
  saveObjects("bc.pnts.boreal.raster.stack", rdata.path)
  rm(bc.pnts.boreal.raster.stack)
  
  bcab.pnts.boreal.raster.stack = stack(sfClusterApplyLB(bcab.pnts.boreal, function(x) rasterize(x=x, y=west.boreal.raster, field=x@data$NUM_TREES, fun="sum")))
  names(bcab.pnts.boreal.raster.stack) = names(bcab.pnts.boreal)
  saveObjects("bcab.pnts.boreal.raster.stack", rdata.path)
  rm(bcab.pnts.boreal.raster.stack)
sfStop()
