
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

### RASTERS
loadObjects(c("bc.poly.boreal.raster.stack", "bc.poly.boreal.raster.stack",
              "bcab.poly.boreal.raster.stack", "west.boreal.raster"), rdata.path)

newPlot()
colours <- brewer.pal(n=9, name="YlOrRd")
# only 9 cols so only plot 9
last9 <- (length(names(bc.poly.boreal.raster.stack))-8):length(names(bc.poly.boreal.raster.stack))
plot(west.boreal)
for (i in rev(last9)) {
  # plot them in reverse order to better see colours
  plot(bc.poly.boreal.raster.stack[[i]], col=colours[i-last9[1]], legend=FALSE, add=TRUE)
  
}
legend("topright", legend=names(bc.poly.boreal.raster.stack[[last9]]), fill=colours)
