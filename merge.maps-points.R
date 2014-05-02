###
### LOAD WORKSPACE SETTINGS
###   - make sure `num.cpus` is set
if (!exists("WORKSPACE")) source("workspace.maps.R")
if(!exists("crs.boreal")) {
  loadObjects("boreal", rdata.path)
  crs.boreal = CRS(proj4string(boreal))
  rm(boreal)
}

### merge ab and bc points
wh.ab = na.omit(pmatch(names(bc.pnts.boreal), names(ab.pnts.boreal)))
wh.bc = na.omit(pmatch(substr(names(ab.pnts.boreal), 1, 4), names(bc.pnts.boreal)))

sfInit(cpus=num.cpus, parallel=TRUE)
  sfLibrary(sp)
  sfExport("ab.pnts.boreal", "bc.pnts.boreal", "crs.boreal", "wh.ab", "wh.bc")  
  
  bcab.pnts.boreal = sfClusterApplyLB(1:length(wh.ab), function(x) {
    out = merge(bc.pnts.boreal[[wh.bc[x]]], ab.pnts.boreal[[wh.ab[x]]], all=TRUE)
    coordinates(out) <- ~ coords.x1 + coords.x2
    out$ntrees = ifelse(!is.na(out$NUM_TREES), out$NUM_TREES, ifelse(!is.na(out$num_trees),out$num_trees, NA))
    return(out)})
  bcab.pnts.boreal = sfClusterApplyLB(bcab.pnts.boreal, function(x) {proj4string(x) <- crs.boreal; return(x)})
  names(bcab.pnts.boreal) = names(bc.pnts.boreal)[wh.bc]
  
  saveObjects("bcab.pnts.boreal", rdata.path)
  rm(ab.pnts.boreal, bc.pnts.boreal)
sfStop()

rm(wh.ab, wh.bc)
