###
### LOAD WORKSPACE SETTINGS
###   - make sure `num.cpus` is set
if (!exists("WORKSPACE")) source("workspace.maps.R")

### PROCESS AB AND BC MAPS (POINTS)
sfInit(cpus=num.cpus, parallel=TRUE)
  sfLibrary(rgdal)
  sfLibrary(sp)
    
  ### AB maps
  ab.pnts.files = dir(path=file.path(maps.dir, "MPB", "ab_mpb"), pattern="spot")
  ab.pnts.dir.shp = unique(sapply(strsplit(ab.pnts.files, "\\."), function(x) x[[1]]))
  ab.pnts = sfClusterApplyLB(ab.pnts.dir.shp, fun=getOGR, dir=file.path(maps.dir, "MPB", "ab_mpb"))
  names(ab.pnts) = substr(sapply(strsplit(ab.pnts.dir.shp,"_"), function(x) x[[3]]), 1, 4)
  
  ### BC maps
  bc.pnts.files = dir(path=file.path(maps.dir, "MPB", "province_BC"), pattern="spot")
  bc.pnts.dir.shp = unique(sapply(strsplit(bc.pnts.files, "\\."), function(x) x[[1]]))
  omit = which(match(bc.pnts.dir.shp, "ibm_spot_data_northernpoints_removed", nomatch=0)==1)
  if(length[omit]>0) bc.pnts.dir.shp = bc.pnts.dir.shp[-omit]
  bc.pnts = sfClusterApplyLB(bc.pnts.dir.shp, fun=getOGR, dir=file.path(maps.dir, "MPB", "province_BC"))
  names(bc.pnts) = sapply(strsplit(bc.pnts.dir.shp,"_"), function(x) x[[3]])

  # save these new map objects for later use
  saveObjects(c("ab.pnts", "bc.pnts"), rdata.path)

  # clean up workspace
  rm(ab.pnts, ab.pnts.dir.shp, ab.pnts.files, bc.pnts, bc.pnts.dir.shp, bc.pnts.files, omit)
sfStop()
