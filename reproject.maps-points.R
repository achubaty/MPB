###
### LOAD WORKSPACE SETTINGS
###   - make sure `num.cpus` is set
if (!exists("WORKSPACE")) source("workspace.maps.R")

### REPROJECT AB, BC SO THEY ARE BOTH IN THE `boreal` PROJECTION
sfInit(cpus=num.cpus, parallel=TRUE)
  sfLibrary(rgdal)
  sfExport("crs.boreal")
  ab.pnts.boreal = sfClusterApplyLB(ab.pnts, spTransform, crs.boreal)
  bc.pnts.boreal = sfClusterApplyLB(bc.pnts, spTransform, crs.boreal)

  names(ab.pnts.boreal) = names(ab.pnts)
  names(bc.pnts.boreal) = names(bc.pnts)

  # save these new map objects for later use
  saveObjects(c("ab.pnts.boreal", "bc.pnts.boreal"), rdata.path)
  
  # cleanup workspace
  rm(ab.pnts.boreal, bc.pnts.boreal)
sfStop()
