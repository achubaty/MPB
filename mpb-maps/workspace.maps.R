###
### SETUP R WORKSPACE TO WORK WITH MPB MAP DATA
###
library("devtools")
source_url("https://raw.githubusercontent.com/achubaty/r-tools/master/load-packages.R")
source_url("https://raw.githubusercontent.com/achubaty/r-tools/master/rdata-objects.R")
source_url("https://raw.githubusercontent.com/achubaty/r-tools/master/sysmem.R")

reqd.pkgs = list("data.table",
                 "magrittr",
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
                 "spatstat",
                 "RColorBrewer")

loadPackages(reqd.pkgs, install=TRUE, quiet=TRUE)

# set work dirs based on computer used
OS = Sys.info()[["sysname"]]
if (OS=="Darwin") {
  maps.dir = "~/Documents/data/maps"
  work.dir = "~/Documents/GitHub/MPB/mpb-maps"
} else if (OS=="Linux") {
  if (pmatch("W-VIC", Sys.info()[["nodename"]], nomatch=0)) {
    maps.dir = "/mnt/A105254/shared/data"
  } else {
    maps.dir = "~/Documents/data"
  }
  work.dir = "~/Documents/GitHub/MPB/mpb-maps"
} else if (OS=="Windows") {
  maps.dir = "//W-VIC-A105254/shared/data"
  work.dir = "~/GitHub/MPB/mpb-maps"
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

num.cpus = 4 # maximum cpus to use

read.in.raw.maps = FALSE
reproj.raw.maps = FALSE
rasterize.maps = FALSE

res.maps = 1000
ext.maps = extent(x=-1027658, xmax=320751.9, ymin=5108872, ymax=6163350)

WORKSPACE = TRUE
