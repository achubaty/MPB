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

west.empty.raster = raster(extent(west.boreal))
res(west.empty.raster) <- res.maps
west.raster = rasterize(west.boreal, west.empty.raster)

### Save these new map objects for later use
saveObjects("west.boreal.raster", rdata.path)

rm(west.boreal.raster, west.empty.raster)
