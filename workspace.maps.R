###
### SETUP R WORKSPACE TO WORK WITH MPB MAP DATA
###
devtools::source_url("https://raw.githubusercontent.com/achubaty/r-tools/master/load-packages.R")
devtools::source_url("https://raw.githubusercontent.com/achubaty/r-tools/master/plot2dev.R")
devtools::source_url("https://raw.githubusercontent.com/achubaty/r-tools/master/rdata-objects.R")
devtools::source_url("https://raw.githubusercontent.com/achubaty/r-tools/master/sysmem.R")

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

OS = Sys.info()[["sysname"]]
if (OS=="Darwin") {
  maps.dir = "~/Documents/data/maps"
  work.dir = "~/Documents/GitHub/MPB"
} else if (OS=="Linux") {
  if (pmatch("W-VIC", Sys.info()[["nodename"]], nomatch=0)) {
    maps.dir = "~/data/maps"
    work.dir = "~/GitHub/MPB"
  } else {
    maps.dir = "~/Documents/data/maps"
    work.dir = "~/Documents/GitHub/MPB"
  }
} else if (OS=="Windows") {
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

WORKSPACE = TRUE
