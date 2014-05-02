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
  rm(ab.poly.boreal)

  bc.poly.boreal = sfClusterApplyLB(bc.poly, spTransform, crs.boreal)
  names(bc.poly.boreal) = names(bc.poly)
  saveObjects("bc.poly.boreal", rdata.path)
  rm(bc.poly.boreal)
sfStop()


###################
wh.ab = na.omit(pmatch(names(bc.poly.boreal), names(ab.poly.boreal)))
wh.bc = na.omit(pmatch(substr(names(ab.poly.boreal),1,4), names(bc.poly.boreal)))

sfInit(cpus=guesstimate, parallel=TRUE)
  sfLibrary(sp)
  sfExport("crs.boreal", "wh.ab", "wh.bc")
  #"ab.poly.boreal", "bc.poly.boreal", 
  bcab.poly.boreal = sfClusterApplyLB(1:length(wh.ab), function(x) {
    out = merge(bc.poly.boreal[[wh.bc[x]]], ab.poly.boreal[[wh.ab[x]]], all=TRUE)
    coordinates(out) <- ~ coords.x1 + coords.x2
    return(out)})
  bcab.poly.boreal = sfClusterApplyLB(bcab.poly.boreal, function(x) {proj4string(x) <- crs.boreal; return(x)})
  names(bcab.poly.boreal) = names(bc.poly.boreal)[wh.bc]
  
  saveObjects("bcab.poly.boreal", rdata.path)
sfStop()

rm(wh.ab, wh.bc)
