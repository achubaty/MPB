###
dev(2)
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

dev(4)
toplot = 2010
wh = match(toplot, years)
plot(west.county,border="light grey")
x = wh
title(years[x])
if (!is.na(any(pmatch(years[x],names(ab.polygon))))) 
  plot(ab.polygon[[pmatch(years[x],names(ab.polygon))]],add = T, col = x,border = x)
points(bcab.ll[[years[x]]][,match(c("coords.x1","coords.x2"),names(bcab.ll[[years[x]]]))],pch=".",col="black")

dev(5)
toplot = 2011
wh = match(toplot, years)
plot(west.county,border="light grey")
x = wh
title(years[x])
if (!is.na(any(pmatch(years[x],names(ab.polygon))))) 
  plot(ab.polygon[[pmatch(years[x],names(ab.polygon))]],add = T, col = x,border = x)
symbols(x = bcab.ll[[years[x]]][,match(c("coords.x1","coords.x2"),names(bcab.ll[[years[x]]]))],
  circles=bcab.ll.ntrees[[years[x]]]$ntrees,col="black",add = T)

legend("topright", legend=toplot, col = wh, pch=19, xpd=F)

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
points(bcab[["2011"]][,1:2], add = TRUE, pch=".")

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
#ras[sample(1:prod(dim(ras)[1:2]),1000,replace=T)] = sample(1:200, 1000, replace=TRUE)

ras.spm = rasterAsSparse(ras)
ras2 = rasterFromSparse(ras.spm,ras)

extract(ras)

cellStats(ras2 != ras,"sum")

rasterAsSparse = function(ras) {
  ras.m = rowColFromCell(cell=Which(ras>=1,cell=T), ras)
  ras.spm = spMatrix(ncol=dim(ras)[2], nrow = dim(ras)[1],
                     i = ras.m[,"row"], j = ras.m[,"col"], x=ras[ras.m])
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
                writeRaster(ras,"test.nc", overwrite=TRUE),
                writeRaster(ras,"test.grd", overwrite=TRUE),
                writeRaster(ras,"test.asc", overwrite=TRUE),
                writeRaster(ras,"test.sdat", overwrite=TRUE) ,
                writeRaster(ras,"test.img", overwrite=TRUE),
                writeRaster(ras,"test1.tif", overwrite=TRUE),
                writeRaster(ras,"test.bil", overwrite=TRUE),
                writeRaster(ras,"test.envi", overwrite=TRUE),
                save(ras,file="test.rdata")
)

rm(ras)
ras1 = raster("test.img")

# Find 1 km scale
#(extent(west.r)@xmax - extent(west.r)@xmin)/1000
#(extent(west.r)@ymax - extent(west.r)@ymin)/1000

#plot(boreal,add = T)
boreal.west = intersect(boreal, west)

boreal = boreal3
plot(boreal[boreal$HA>1e6 & boreal@data$TYPE=="BOREAL",], col = boreal@data$TYPE)
plot(canada1.boreal, add = TRUE)

rasterize()

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

