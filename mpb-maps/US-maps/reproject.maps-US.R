###
### LOAD WORKSPACE SETTINGS
###   - make sure `num.cpus` is set
if (!exists("WORKSPACE")) source("workspace.maps.R")
if(!exists("crs.boreal")) {
  loadObjects("boreal", rdata.path)
  crs.boreal = CRS(proj4string(boreal))
  rm(boreal)
}

### REPROJECT US MAPS SO THEY ARE BOTH IN THE `boreal` PROJECTION
sfInit(cpus=num.cpus, parallel=TRUE)
  sfLibrary(rgdal)
  sfExport("us.poly", "crs.boreal")
  years.post = names(us.poly)
  years.pre = unique(us.poly.pre2006$YEAR)
  for (year in years.pre) {
    ids = which(us.poly.pre2006$YEAR == year)
    us.poly = append(us.poly.pre2006[ids,], us.poly)
  }
  us.poly.boreal = sfClusterApplyLB(us.poly, spTransform, crs.boreal)
  names(us.poly.boreal) = c(rev(years.pre), years.post)
  saveObjects("us.poly.boreal", rdata.path)
  rm(us.poly.boreal, years.pre, years.post)
sfStop()
