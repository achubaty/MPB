###
### LOAD WORKSPACE SETTINGS
###   - make sure `num.cpus` is set
if (!exists("WORKSPACE")) source("workspace.maps.R")

### PROCESS AB AND BC MAPS (POINTS)
sfInit(cpus=num.cpus, parallel=TRUE)
  sfLibrary(rgdal)
  sfLibrary(sp)
  
  ### US maps
  us.poly.files = dir(path=file.path(maps.dir, "MPB", "US"), pattern="us_mpb")
  us.poly.dir.shp = unique(sapply(strsplit(us.poly.files, "\\."), function(x) x[[1]]))
  us.poly = sfClusterApplyLB(us.poly.dir.shp, fun=getOGR, dir=file.path(maps.dir, "MPB", "US"))
  names(us.poly) = sapply(strsplit(us.poly.dir.shp,"mpb"), function(x) x[[2]])
  
  us.poly.pre2006 = us.poly[["1997to2005"]] # no 2005?
  us.poly = us.poly[-1] # drop "1997to2005" from the list
  
  years.post = names(us.poly)
  years.pre = unique(us.poly.pre2006$YEAR)
  for (year in years.pre) {
    ids = which(us.poly.pre2006$YEAR == year)
    us.poly = append(us.poly.pre2006[ids,], us.poly)
  }
  names(us.poly) = c(rev(years.pre), years.post)
  saveObjects("us.poly", rdata.path)
sfStop()
