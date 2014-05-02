###
### LOAD WORKSPACE SETTINGS
###   - make sure `num.cpus` is set
if (!exists("WORKSPACE")) source("workspace.maps.R")
if(!exists("crs.boreal")) {
  loadObjects("boreal", rdata.path)
  crs.boreal = CRS(proj4string(boreal))
  rm(boreal)
}

loadObjects(c("ab.poly.boreal.raster.stack", "bc.poly.boreal.raster.stack"), rdata.path)

### merge ab and bc polygons
ab.poly.boreal.raster.unstack = unstack(ab.poly.boreal.raster.stack)
bc.poly.boreal.raster.unstack = unstack(bc.poly.boreal.raster.stack)

bcab.poly.boreal.raster.unstack = bc.poly.boreal.raster.unstack

wh.ab = na.omit(match(names(bc.poly.boreal.raster.stack), names(ab.poly.boreal.raster.stack)))
wh.bc = na.omit(match(names(ab.poly.boreal.raster.stack), names(bc.poly.boreal.raster.stack)))
bcab.poly.boreal.raster.unstack[wh.bc] = lapply(1:length(wh.bc), function(x) {
  out = merge(bc.poly.boreal.raster.unstack[[wh.bc[x]]], ab.poly.boreal.raster.unstack[[wh.ab[x]]])
  return(out)})
bcab.poly.boreal.raster.stack = stack(bcab.poly.boreal.raster.unstack)
names(bcab.poly.boreal.raster.stack) = names(bc.poly.boreal.raster.stack)

saveObjects("bcab.poly.boreal.raster.stack", rdata.path)

objects2rm <- c("ab.poly.boreal.raster.stack", "ab.poly.boreal.raster.unstack",
                "bc.poly.boreal.raster.stack", "bc.poly.boreal.raster.unstack",
                "bcab.poly.boreal.raster.stack", "bcab.poly.boreal.raster.unstack", 
                "wh.ab", "wh.bc")
rm(list=objects2rm)
rm(objects2rm)

# manual garbage collection to free recently unallocated memory
for (i in 1:10) gc()
