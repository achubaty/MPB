library(maptools)
library(maps)
library(mapdata)
library(plotKML)
library(RColorBrewer)

### set up workspace
num.cpus = 4

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

devtools::source_url("https://raw.githubusercontent.com/achubaty/r-tools/master/newplot.R")

# Load the precollected R files instead
path = file.path(maps.dir, "MPB", "Rmaps")
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


