
# Load the precollected R files instead
mpb.points = c("ab", "bc", "west")
mpb.polygons = c("ab.poly", "bc.poly", "us.poly", "west")
mpb.rasters = c("ab.r", "ab.poly.r", "bc.r", "bc.poly.r", "us.poly.r", "west.r")

objects2load = mpb.rasters
lapply(objects2load, function(x) load(file=paste(path, "/", x, ".rdata", sep=""), env=globalenv()))


############


### AB and BC points data
newPlot()
plot(west)
colours <- brewer.pal(n=9, name="YlOrRd")
for (i in names(ab)) {
  points(ab[[i]], pch=".", col=colours)
}
for (i in names(bc)) {
  points(bc[[i]], pch=".", col=colours)
}

### AB and BC polygon data
newPlot()
plot(west)
colours <- brewer.pal(n=9, name="YlOrRd")
plot(ab.poly)
plot(bc.poly)
#plot(us.poly)

### RASTERS
newPlot()
plot(west.r)
colours <- brewer.pal(n=9, name="YlOrRd")


