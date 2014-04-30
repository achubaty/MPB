### Useful resources:
###   http://www.nyu.edu/projects/politicsdatalab/workshops/GISwR.pdf

################################################################################

### load required packages
library(data.table)
library(maptools)
library(maps)
library(mapdata)
library(plotKML)
library(rgeos)
library(rts)
library(shapefiles)
library(snowfall)

### set up workspace
num.cpus = 2

if (Sys.info()[["sysname"]]=="Darwin") {
  maps.dir = "~/Documents/data/maps"
  work.dir = "~/Documents/GitHub/MPB"
} else if (Sys.info()[["sysname"]]=="Linux") {
  maps.dir = "~/Documents/data/maps"
  work.dir = "~/Documents/GitHub/MPB"
} else if (Sys.info()[["sysname"]]=="Windows") {
  maps.dir = "~/data/maps"
  work.dir = "~/GitHub/MPB"
} else {
  print("Which operating system are you using?")
}

setwd(work.dir)

read.in.raw.maps = FALSE
work.on.raw.maps = FALSE

devtools::source_url("https://raw.githubusercontent.com/achubaty/r-tools/master/newplot.R")

### LOAD MPB DATA FROM BC AND AB, BOTH POLYGON AND POINT DATA SOURCES
path = file.path(maps.dir, "MPB", "Rmaps")
if (read.in.raw.maps) {
  # Read in raw maps, and save them as individual files (takes ~10 mins)
  source(file.path(work.dir, "import.raw.maps.R"))
} else {
  # Load the precollected R files instead
  objects2load = c("ab", "ab.poly", "bc", "bc.poly", "boreal", "us.poly", "west", "west.county", "west.r")
  lapply(objects2load, function(x) load(file=paste(path, "/", x, ".rdata", sep=""), env=globalenv()))
}

### Convert everything to rasters @ 1 ha resolution.
###   This resolution decision is determined with the `west.r` rasterization done when importing

if (work.on.raw.maps) {
  sfInit(cpus=num.cpus, parallel=TRUE)
    sfLibrary(raster)
    sfExport("west.r")
    
    ab.r = stack(sfClusterApplyLB(ab, function(x) rasterize(x=x, y=west.r,
                field=if(any(colnames(x@data)=="NUM_TREES")) x@data$NUM_TREES else x@data$num_trees, fun="sum")))
    names(ab.r) = sapply(names(ab), function(x) strsplit(x,"spot")[[1]])
    save(ab.r, file=file.path(path, "ab.r.rdata"))
  
    bc.r = stack(sfClusterApplyLB(bc, function(x) rasterize(x=x, y=west.r, field=x@data$NUM_TREES, fun="sum")))
    names(bc.r) = names(bc)
    save(bc.r, file=file.path(path, "bc.r.rdata"))
  sfStop()
  
  ### merge alberta and bc points
  sfInit(cpus=num.cpus, parallel=TRUE)
    sfLibrary(sp)
    
    wh.ab = na.omit(pmatch(names(bc), names(ab)))
    wh.bc = na.omit(pmatch(substr(names(ab), 1, 4), names(bc)))
    
    sfExport("ab", "bc", "west.r", "wh.ab", "wh.bc")
      
    bcab = sfClusterApplyLB(1:length(wh.ab), function(x) {
              out = merge(bc[[wh.bc[x]]], ab[[wh.ab[x]]], all=TRUE)
              coordinates(out) <- ~ coords.x1 + coords.x2
              out$ntrees = ifelse(!is.na(out$NUM_TREES), out$NUM_TREES, ifelse(!is.na(out$num_trees),out$num_trees, NA))
              return(out)})
    bcab = sfClusterApplyLB(bcab, function(x) {proj4string(x) <- proj4string(west.r); return(x)})
    names(bcab) = names(bc)[wh.bc] #sapply(strsplit(bc.dir.shp,"_"),function(x) x[[3]])[wh.bc]
    rm(wh.ab, wh.bc)
    
    save(bcab, file=file.path(path, "bcab.rdata"))
    rm(ab, bc)
  sfStop()

  sfInit(cpus=num.cpus, parallel=TRUE)
    sfLibrary(sp)

    wh.ab.poly = na.omit(pmatch(names(bc.poly), names(ab.poly)))
    wh.bc.poly = na.omit(pmatch(substr(names(ab.poly),1,4), names(bc.poly)))
    
    sfExport("ab.poly", "bc.poly", "west.r", "wh.ab.poly", "wh.bc.poly")
        
    bcab.poly = sfClusterApplyLB(1:length(wh.ab.poly), function(x) {
      out = merge(bc.poly[[wh.bc.poly[x]]], ab.poly[[wh.ab.poly[x]]], all=TRUE)
      coordinates(out) <- ~ coords.x1 + coords.x2
      return(out)})
    bcab.poly = sfClusterApplyLB(bcab, function(x) {proj4string(x) <- proj4string(west.r); return(x)})
    names(bcab.poly) = names(bc.poly)[wh.bc.poly] #sapply(strsplit(bc.dir.shp,"_"),function(x) x[[3]])[wh.bc]
    rm(wh.ab.poly, wh,bc,poly)
    
    save(bcab.poly, file=file.path(path, "bcab.poly.rdata"))
  sfStop()
  
  rm(bcab)
  
  ### for the next two, we arbitrarily picked 1000 trees per 1ha
  ###   This NEEDS to be revisited.
  change.res = function(x, y=west.r, field=1000, fun="last", ...) {
    rasterize(x=x, y=y, field=field, fun=fun, ...)
  }
  
  sfInit(cpus=num.cpus, parallel=TRUE)
    sfLibrary(raster)
    sfExport("west.r")
    
    ab.poly.r.stack = stack(sfClusterApplyLB(ab.poly, change.res))
    names(ab.poly.r.stack) = sapply(names(ab.poly), function(x) strsplit(x,"poly")[[1]])
  
    # several rasters have no values because they were in southern Alberta: `west.r` doesn't cover that.
    nas = which(sapply(1:nlayers(ab.poly.r.stack), function(x) unique(!is.na(which.min(ab.poly.r.stack[[x]])))))
    ab.poly.r = ab.poly.r.stack[[nas]] # remove the NA layers
    names(ab.poly.r) = unlist(strsplit(names(ab.poly),"poly"))[nas]
    save(ab.poly.r, file=file.path(path, "ab.poly.r.rdata"))
    rm(ab.poly.r.stack, nas)
    
    bc.poly.r = stack(sfClusterApplyLB(bc.poly, change.res))
    names(bc.poly.r) = names(bc.poly)
    save(bc.poly.r, file=file.path(path, "bc.poly.r.rdata"))
  sfStop()
} else {
  load(file.path(path, "ab.r.rdata"))
  load(file.path(path, "bc.r.rdata"))
  load(file.path(path, "ab.poly.r.rdata"))
  load(file.path(path, "bc.poly.r.rdata"))
  load(file.path(path, "bcab.rdata"))
  load(file.path(path, "bcab.poly.rdata"))
}

####################################################################

# combine bc.poly and ab.poly:
bc.poly.r.us = unstack(bc.poly.r)
ab.poly.r.us = unstack(ab.poly.r)
poly.r.us = bc.poly.r.us

wh.poly.bc =  na.omit(match(names(ab.poly.r), names(bc.poly.r)))
wh.poly.ab =  na.omit(match(names(bc.poly.r), names(ab.poly.r)))
poly.r.us[wh.poly.bc] = lapply(1:length(wh.poly.ab), function(x) {
                                out = bc.poly.r.us[[wh.poly.bc[x]]] + ab.poly.r.us[[wh.poly.ab[x]]]
                                return(out)})
poly.r = stack(poly.r.us)
names(poly.r) = names(bc.poly.r)

# assigning names to layers isn't working
#names(bc.r) = names(bcab)
#names(ab.poly.r) = unlist(strsplit(names(ab.poly),"poly"))
#names(bc.poly.r) = names(bc.poly)

wh.poly.r = na.omit(match(substr(names(ab.poly),1,4), substr(names(poly.r),2,5)))
wh.bcab.r = na.omit(pmatch(substr(names(ab.poly),1,4), names(bcab)))

all = list()
inner.count = 0
for (i in 1:nlayers(bc.r)) {
  if (any(substr(names(bc.r),2,5)[i] == substr(names(poly.r),2,5))) {
    inner.count = inner.count + 1
    if (any(is.finite(cellStats(poly.r[[i]], "range")))) {
        all[[i]] = bc.r[[wh.bcab.r[inner.count]]] + poly.r[[wh.poly.r[inner.count]]]
    } else {
        all[[i]] = bc.r[[wh.bcab.r[inner.count]]]
    }
  } else {
    all[[i]] = bc.r[[i]]
  }
}

all = lapply(all, function(x) { x[is.na(x)] <- 0; return(x) })
all = lapply(all, function(x) { x[x>0] <- log(x[x>0])+10; return(x) })


stk = stack(all)
brk = brick(all)
#brk = aggregate(stk, fact=1)

latlongproj = "+proj=longlat"
brk.ll = projectRaster(brk, crs=latlongproj)
brk.ll = brick(lapply(1:nlayers(brk.ll), function(x) {brk.ll[[x]][is.na(brk.ll[[x]])]<- 0; return(brk.ll[[x]])}))
brk.ll@title <- "MPB intensity"

years = 2001:2012
x1 = paste(years[1:(length(years)-1)],"-10-01",sep="")
x2 = paste(years[2:length(years)],"-10-01",sep="")
z1 = as.POSIXct(as.Date(x1))
z2 = as.POSIXct(as.Date(x2))

sps = SpatialPointsDataFrame(spTransform(spsample(west, 1, type="random"), CRS(latlongproj)), data=data.frame(dat=1))
ts = new("RasterBrickTimeSeries", variable="X", sampled=sps, rasters=brk.ll, TimeSpan.begin=z1, TimeSpan.end=z2)
ts2 = rts(brk.ll, time=z1)
dims = dim(brk.ll)
plotKML(ts, colour_scale = c(rep("black",2),heat.colors(12)[12:1]), pngwidth = dims[1], pngheight = dims[2], pngpointsize = 14)

names(all) = names(bc.r)
plot(all)

wind(2)
par(mfrow = c(3,3))
par(omi = c(0.01, 0.01, 0.01, 0.01))
par(mai = c(0, 0, 0.1, 0))
years = names(bcab.ll)
toplot = 2003:2011
wh = match(toplot, years)
west.county.ll = reproject(west.county)
for (x in wh) {
  plot(west.county.ll, border="light grey")
  title(years[x])
#  points(bcab.ll[[years[x]]][,match(c("coords.x1","coords.x2"),names(bcab.ll[[years[x]]]))],pch=".",col="black")
  symbols(x = coordinates(bcab.ll[[years[x]]]),#bcab.ll[[years[x]]][,match(c("coords.x1","coords.x2"),names(bcab.ll[[years[x]]]))],
    circles=bcab.ll[[years[x]]]$ntrees/1e4,col="black",add = T, inches = F)

  if (!is.na(any(pmatch(years[x],names(ab.polygon))))) 
    plot(ab.polygon[[pmatch(years[x],names(ab.polygon))]],add = T, col = x,border = x)
}

wind(4)
toplot = 2010
wh = match(toplot, years)
plot(west.county,border="light grey")
x = wh
title(years[x])
if (!is.na(any(pmatch(years[x],names(ab.polygon))))) 
  plot(ab.polygon[[pmatch(years[x],names(ab.polygon))]],add = T, col = x,border = x)
points(bcab.ll[[years[x]]][,match(c("coords.x1","coords.x2"),names(bcab.ll[[years[x]]]))],pch=".",col="black")

wind(5)
toplot = 2011
wh = match(toplot, years)
plot(west.county,border="light grey")
x = wh
title(years[x])
if (!is.na(any(pmatch(years[x],names(ab.polygon))))) 
  plot(ab.polygon[[pmatch(years[x],names(ab.polygon))]],add = T, col = x,border = x)
symbols(x = bcab.ll[[years[x]]][,match(c("coords.x1","coords.x2"),names(bcab.ll[[years[x]]]))],
  circles=bcab.ll.ntrees[[years[x]]]$ntrees,col="black",add = T)

legend("topright",legend=toplot, col = wh,pch=19,xpd=F)

sapply(wh, function(x) points(bcab.ll[[years[x]]][, match(c("coords.x1","coords.x2"), names(bcab.ll[[years[x]]]))], pch=".", col=x))

points(bcab.ll[["2011"]][,1:2],pch=".",col="red")
points(bcab.ll[["2010"]][,1:2],pch=".",col="green")

lapply(1:length(ab.ll), function(x) plot(ab.ll[[x]], add=T, pch=".", col=x))
plot(mpb2011,add = T,pch = ".",col="red")
plot(usmpb2011,add =T, pch = ".",col="red")
plot(abmpb2011,add =T, pch = ".",col="red")

###################################################################################
al = AgentLocation(Which(west.r==2) )



al = AgentLocation(ab.poly[[17]])
pri = ProbInit(map = ab.poly[[17]],p = sapply(ab.poly[[17]]@polygons, function(x) x@area) )
na = NumAgents(1e3)                                                    

mpb = new("agent", agentlocation=al, numagents = na, probinit = pri)


al = AgentLocation(bc.r[[5]])    
pri = ProbInit(map = ab.poly[[11]],p = 1, function(x) x@area) 
na = NumAgents(1e4)

mpb = new("agent", agentlocation=al, numagents = na)#, numagents = na) #probinit = pri, 


transitions()





plot(west)
points(bcab[["2011"]][,1:2],add = T, pch=".")

points(mpb,pch=".")


plot(west)
#ext = drawExtent()
ext = extent(x= -937658, xmax = 320751.9 , ymin = 5108872 , ymax = 6163350 )

west.empty = raster(ext)
res(west.empty) <- 1000
west.r = rasterize(west,west.empty)

plot(west.r)

west.boreal = crop(boreal,extent(west.r))
plot(boreal[boreal$HA>1e6 & boreal@data$TYPE=="BOREAL",], col = boreal@data$TYPE)



# Sparse raster
library(Matrix)

ras = Which(west.r>1)
#ras[sample(1:prod(dim(ras)[1:2]),1000,replace=T)] = sample(1:200,1000,replace=T)


ras.spm = rasterAsSparse(ras)
ras2 = rasterFromSparse(ras.spm,ras)

extract(ras


cellStats(ras2 != ras,"sum")





rasterAsSparse = function(ras) {
  ras.m = rowColFromCell(cell=Which(ras>=1,cell=T),ras)
  ras.spm = spMatrix(ncol=dim(ras)[2], nrow = dim(ras)[1], i = ras.m[,"row"], j = ras.m[,"col"],x=ras[ras.m])
  return(ras.spm)
}
  
rasterFromSparse = function(sp.ras, ras) {
  return.ras = raster(as.matrix(ras.spm))
  extent(return.ras) = extent(ras)
  crs(return.ras) = crs(ras)
  return(return.ras)
}

setwd("c:/Rwork")

ben = benchmark(replications= 1,
writeRaster(ras,"test.nc",overwrite=T),
writeRaster(ras,"test.grd",overwrite=T),
writeRaster(ras,"test.asc",overwrite=T),
writeRaster(ras,"test.sdat",overwrite=T) ,
writeRaster(ras,"test.img",overwrite=T)   ,
writeRaster(ras,"test1.tif",overwrite=T)   ,
writeRaster(ras,"test.bil",overwrite=T)     ,
writeRaster(ras,"test.envi",overwrite=T),
save(ras,file="test.rdata")     )





rm(ras)
ras1 = raster("test.img")





# Find 1 km scale
#(extent(west.r)@xmax - extent(west.r)@xmin)/1000
#(extent(west.r)@ymax - extent(west.r)@ymin)/1000


#plot(boreal,add = T)
boreal.west = intersect(boreal,west)


boreal = boreal3
plot(boreal[boreal$HA>1e6 & boreal@data$TYPE=="BOREAL",], col = boreal@data$TYPE)
plot(canada1.boreal,add = T)

rasterize(


##################Other

# if you have a data.frame with coordinates as two columns, just use function coordinates() to make it a
#  SpatialPointsDataFrame

data(meuse)
coordinates(meuse) <- c("x","y")
proj4string(meuse) <- CRS("+init=epsg:28992")




setwd("c:/Rwork/MPB/province_BC")
mpb2011.imported = readOGR(dsn=".", layer = "ibm_spot_2011")
mpb2011 = spTransform(mpb2011.imported, CRS(proj4string(boreal)))

setwd("c:/Rwork/MPB/US")
us.mpb2011.imported = readOGR(dsn=".", layer = "us_mpb2011")
usmpb2011 = spTransform(us.mpb2011.imported, CRS(proj4string(boreal)))

