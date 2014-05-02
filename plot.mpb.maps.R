
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
newPlot()
plot(west.r)
colours <- brewer.pal(n=9, name="YlOrRd")


