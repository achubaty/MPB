###
### LOAD WORKSPACE SETTINGS
###   - make sure `num.cpus` is set
source("workspace.maps.R")

### REPROJECT AB, BC, US SO THEY ARE BOTH IN THE `boreal` PROJECTION
sfInit(cpus=num.cpus, parallel=TRUE)
  sfLibrary(rgdal)
  sfExport("crs.boreal")
  ab.poly.boreal = sfClusterApplyLB(ab.poly, spTransform, crs.boreal)
  bc.poly.boreal = sfClusterApplyLB(bc.poly, spTransform, crs.boreal)
sfStop()

names(ab.poly.boreal) = names(ab.pnts)
names(bc.poly.boreal) = names(bc.pnts)

# save these new map objects for later use
saveObjects(c("ab.poly.boreal", "bc.poly.boreal"), rdata.path)
rm(ab.poly.boreal, bc.poly.boreal)
