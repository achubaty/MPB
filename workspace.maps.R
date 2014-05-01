###
### SETUP R WORKSPACE TO WORK WITH MPB MAP DATA
###
devtools::source_url("https://raw.githubusercontent.com/achubaty/r-tools/master/plot2dev.R")
devtools::source_url("https://raw.githubusercontent.com/achubaty/r-tools/master/load-packages.R")

reqd.pkgs = list("data.table",
                 "maps",
                 "mapdata",
                 "maptools",
                 "plotKML",
                 "raster",
                 "rgdal",
                 "rgeos",
                 "rts",
                 "shapefiles",
                 "snowfall",
                 "RColorBrewer")
load.packages(reqd.pkgs, install=TRUE)

if (Sys.info()[["sysname"]]=="Darwin") {
  maps.dir = "~/Documents/data/maps"
  work.dir = "~/Documents/GitHub/MPB"
} else if (Sys.info()[["sysname"]]=="Linux") {
  if (pmatch("W-VIC", Sys.info()[["nodename"]], nomatch=0)) {
    maps.dir = "~/data/maps"
    work.dir = "~/GitHub/MPB"
  } else {
    maps.dir = "~/Documents/data/maps"
    work.dir = "~/Documents/GitHub/MPB"
  }
} else if (Sys.info()[["sysname"]]=="Windows") {
  maps.dir = "~/data/maps"
  work.dir = "~/GitHub/MPB"
} else {
  print("Which operating system are you using?")
}
setwd(work.dir)
rdata.path = file.path(maps.dir, "MPB", "Rmaps")

getOGR <- function(layer, dir) {
  orig.dir = getwd()
  setwd(dir)
  out = readOGR(dsn=".", layer=layer)
  setwd(orig.dir)
  return(out)
}

loadObjects <- function(objects, path) {
  lapply(objects, function(x) load(file=paste(rdata.path, "/", x, ".RData", sep=""), env=globalenv()))
}

saveObjects <- function(objects, path) {
  lapply(objects, function(x) save(list=x, file=paste(path, "/", x, ".RData", sep="")))
}

WORKSPACE = TRUE
