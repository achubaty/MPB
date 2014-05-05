###
### LOAD WORKSPACE SETTINGS
###   - cpus is set to `guesstimate` instead of `num.cpus`
###   - make sure `ext.maps` is set
###   - make sure `res.maps` is set
if (!exists("WORKSPACE")) source("workspace.maps.R")

if(!exists("crs.boreal")) {
  loadObjects("boreal", rdata.path)
  crs.boreal = CRS(proj4string(boreal))
  rm(boreal)
}

