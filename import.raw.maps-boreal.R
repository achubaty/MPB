###
### LOAD WORKSPACE SETTINGS
###   - make sure `num.cpus` is set
source("workspace.maps.R")

### LOAD OTHER MAPS, PROVINCE OUTLINES, COUNTY OUTLINES, BOREAL FOREST
boreal <- getOGR("NABoreal", file.path(maps.dir, "boreal"))
crs.boreal = CRS(proj4string(boreal))

load(file.path(maps.dir, "CAN_adm1.RData"))
canada1 = gadm
canada1.boreal = spTransform(canada1, crs.boreal)
west.boreal = canada1.boreal[na.omit(match(c("Alberta", "British Columbia","Saskatchewan"), canada1.boreal$NAME_1)),]
rm(gadm, canada1)

load(file.path(maps.dir, "CAN_adm2.RData"))
canada2 = gadm
canada2.boreal = spTransform(canada2, crs.boreal)
canada2.boreal.dt = data.table(data.frame(canada2.boreal))
setkey(canada2.boreal.dt, "NAME_1")
subset = match(canada2.boreal.dt[c("Alberta", "British Columbia", "Saskatchewan")]$PID, canada2.boreal@data$PID)
# `subset` produces data.table warning re: mixed font encodings
#
west2.boreal = canada2.boreal[subset,]
rm(gadm, canada2, canada2.boreal.dt, subset)

boreal.can <- boreal[boreal$COUNTRY=="CANADA",]
west.boreal.union <- unionSpatialPolygons(west.boreal, west.boreal$ID_0, threshold=1) # threshold value???
boreal.west <- gIntersection(west.boreal.union, boreal.can) # ERROR MESSAGE BELOW

#   Error in RGEOSBinTopoFunc(spgeom1, spgeom2, byid, id, drop_not_poly, "rgeos_intersection") : 
#     TopologyException: Input geom 0 is invalid: Too few points in geometry component at or
#     near point 199577.44732574001 5470112.4132905202 at 199577.44732574001 5470112.4132905202

rm(west.boreal.union)

### Save these new map objects for later use
objects2save = c("boreal", "boreal.can", "boreal.west",
                 "canada1.boreal", "canada2.boreal",
                 "west.boreal", "west2.boreal")
saveObjects(objects2save, rdata.path)
rm(list=objects2save)
