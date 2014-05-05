
# Load the precollected R files instead
mpb.points = c("ab.pnts.boreal", "bc.pnts.boreas", "west.boreal")
mpb.polygons = c("ab.poly.boreal", "bc.poly.boreal", "west.boreal")
mpb.rasters = c("ab.pnts.raster", "ab.poly.raster",
                "bc.pnts.raster", "bc.poly.raster",
                "west.boreal.raster")

loadObjects(mpb.rasters)


############


### AB and BC points data
newPlot()
plot(west)
colours <- brewer.pal(n=9, name="YlOrRd")
for (i in names(ab.pnts.boreal)) {
  points(ab.pnts.boreal[[i]], pch=".", col=colours)
}
for (i in names(bc.pnts.boreal)) {
  points(bc.pnts.boreal[[i]], pch=".", col=colours)
}

### AB and BC polygon data
loadObjects(c("ab.poly.boreal", "bc.poly.boreal", "west.boreal"), rdata.path)
newPlot()
plot(west.boreal)
colours <- brewer.pal(n=9, name="YlOrRd")
for (i in names(bc.poly.boreal)) {
  plot(ab.poly.boreal[[i]], col=colours, add=TRUE)
}
for (i in names(bc.poly.boreal)) {
  plot(bc.poly.boreal[[i]], col=colours, add=TRUE)
}

### POINTS RASTERS






### POLYGON RASTERS
loadObjects(c("ab.poly.boreal.raster.stack", "bc.poly.boreal.raster.stack",
              "bcab.poly.boreal.raster.stack", "west.boreal"), rdata.path)

colours <- brewer.pal(n=9, name="YlOrRd") # only 9 colours in this pallette so only plot 9
last9.bc <- (length(names(bc.poly.boreal.raster.stack))-8):length(names(bc.poly.boreal.raster.stack))
last9.bc <- last9.bc[last9.bc>0]
wh.ab = na.omit(match(names(bc.poly.boreal.raster.stack), names(ab.poly.boreal.raster.stack)))
wh.bc = na.omit(match(names(ab.poly.boreal.raster.stack), names(bc.poly.boreal.raster.stack)))

newPlot()
plot(west.boreal)
for (i in rev(last9.bc)) {
  # plot them in reverse order to better see colours
  plot(bc.poly.boreal.raster.stack[[i]], col=colours[i-last9.bc[1]+1], legend=FALSE, add=TRUE)
  if (i %in% wh.bc) plot(ab.poly.boreal.raster.stack[[wh.ab[which(wh.bc==i)]]], col=colours[i-last9.bc[1]+1], legend=FALSE, add=TRUE)
}
legend("topright", legend=names(bc.poly.boreal.raster.stack[[last9.bc]]), fill=colours)
rm(wh.ab, wh.bc)



colours <- brewer.pal(n=9, name="YlOrRd") # only 9 colours in this pallette so only plot 9
last9 <- (length(names(bcab.poly.boreal.raster.stack))-8):length(names(bcab.poly.boreal.raster.stack))
last9 <- last9[last9>0]

newPlot()
plot(west.boreal)
for (i in rev(last9)) {
  # plot them in reverse order to better see colours
  plot(bcab.poly.boreal.raster.stack[[i]], col=colours[i-last9[1]+1], legend=FALSE, add=TRUE)
}
legend("topright", legend=names(bcab.poly.boreal.raster.stack[[last9]]), fill=colours)



newPlot()
for (i in length(names(bcab.all.raster.stack))) {
  plot(bcab.all.raster.stack[[i]])
  plot(west.boreal, add=TRUE)
}
