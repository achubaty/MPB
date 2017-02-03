library(magrittr)
library(raster)
library(rgdal)
library(snowfall)
library(sp)
library(stringr)

## compute environment variables
._CFS_. <- grepl("W-VIC", Sys.info()[["nodename"]])  ## logical: is this a CFS machine?
._NUM_CPUS_. <- parallel::detectCores() / 2          ## max cpus to use
._OS_. <- Sys.info()[["sysname"]]
._USER_. <- Sys.info()[["user"]]

## MPB data processing options
._RES_MAPS_. <- 250  ## raster resolution (metres)

## set R and knitr options
if (isTRUE(._CFS_.)) {
  options(repos = c(CRAN = "https://cran.rstudio.com/",
                    NRCRAN = "http://132.156.149.95"))
} else {
  options(repos = c(CRAN = "https://cran.rstudio.com/"))
}

## set work dirs based on computer used
if (._USER_. == "achubaty") {
  if (._OS_. == "Darwin") {
    maps.dir <- "~/Documents/shared"
    work.dir <- "~/Documents/GitHub/MPB/mpb-maps"
  } else if (._OS_. == "Linux") {
    if (isTRUE(._CFS_.)) {
      maps.dir <- "/mnt/A105388/shared/data"
    } else {
      maps.dir <- "~/Documents/Data/shared"
    }
    work.dir <- "~/Documents/GitHub/MPB/mpb-maps"
  } else if (._OS_. == "Windows") {
    maps.dir <- "//W-VIC-A105388/shared/data"
    work.dir <- "~/GitHub/MPB/mpb-maps"
  } else {
    stop("Which operating system are you using?")
  }
}
setwd(work.dir)
rdata.path <- file.path(maps.dir, "MPB", "Rmaps")
fig.path <- file.path(work.dir, "figures")
if (!dir.exists(fig.path)) dir.create(fig.path)

stopifnot(all(dir.exists(c(fig.path, maps.dir, rdata.path, work.dir))))

# check that gdal driver exists to open gdb file
if (NROW(subset(ogrDrivers(), grepl("GDB", name))) == 0) {
  stop('The OpenFileGDB driver is not available on your system.')
}

## helper functions:

# imports 'dl.data' to provide robust downloading with file checksumming
source_url("https://raw.githubusercontent.com/achubaty/r-tools/master/download-data.R")

#' manual garbage collection to free recently unallocated memory 
.cleanup <- function() {
  for (i in 1:10) gc()
}

##  file originally downloaded from:
##    ftp://ftp.env.gov.ab.ca/pub/MPB/Data/MPB_AERIAL_SURVEY.gdb.7z
##
##  [ password protected archive, downloaded October 19, 2016                       ]
##  [ see email from Aaron McGill (Aaron.McGill@gov.ab.ca) forwarded by Barry Cooke ]
##
f <- file.path(maps.dir, "MPB/MPB_AERIAL_SURVEY.gdb")
stopifnot(file.exists(f))

# List all feature classes in a file geodatabase
fc_list <- ogrListLayers(f)
print(fc_list)

# each layer represents data for a single year (2 of the last 3 characters in the name)
years <- str_sub(fc_list, 10, 12)
years <- ifelse(years < 50, paste0(20, years), paste0(19, years))

# Read the feature class
layers <- structure( vector("list", length(years)) , names = years)
i <- 1
lapply(fc_list, function(x) {
  fc <- readOGR(dsn = file, layer = x)
  layers[[i]] <<- fc
  i <<- i + 1
})

# Determine the FC extent, projection, and attribute information
lapply(layers, summary)

# Extract points and polygon data into separate lists
id_pnts <- which(substring(names(layers), 5, 5) == "x")
id_poly <- which(substring(names(layers), 5, 5) == "p")

ab_pnts <- layers[id_pnts]
ab_poly <- layers[id_poly]

# order by year
ab_pnts <- ab_pnts[order(names(ab_pnts))]
ab_poly <- ab_poly[order(names(ab_poly))]

stopifnot(all(sapply(ab_pnts, is, class2 = "SpatialPointsDataFrame")))
stopifnot(all(sapply(ab_poly, is, class2 = "SpatialPolygonsDataFrame")))
rm(layers)

# reproject the MPB maps to match the boreal.west projection
# (boreal.west is the intersection of boreal forest with AB, BC, SK)
if (!exists("crs.boreal")) {
  load(file.path(rdata.path, "boreal.west.RData"))
  crs.boreal <- CRS(proj4string(boreal.west))
}

sfInit(cpus = ._NUM_CPUS_., parallel = TRUE)
  sfLibrary(magrittr)
  sfLibrary(sp)
  sfLibrary(rgeos)
  sfLibrary(rgdal)
  sfExport("crs.boreal")
  
  ab_pnts_boreal <- sfClusterApplyLB(ab_pnts, function(pnts) {
    pnts.boreal <- pnts %>%
      setNames(toupper(names(pnts))) %>% 
      spTransform(crs.boreal)
    stopifnot(gIsValid(pnts.boreal))
    
    return(pnts.boreal)
  }) %>%
    setNames(names(ab_pnts))
  
  # save these new map objects for later use
  save("ab_pnts_boreal", file = file.path(rdata.path, "ab_pnts_boreal_2016.Rdata"))
sfStop()
.cleanup()

# plot them
dev.new(noRStudioGD = TRUE)
plot(boreal.west)
lapply(ab_pnts_boreal, plot, add = TRUE, col = 'darkred', pch = ".")

## RASTERIZE
load(file.path(rdata.path, "west.boreal.Rdata"))
west.empty.raster <- raster(extent(west.boreal), resolution = ._RES_MAPS_.)
west.boreal.raster <- suppressWarnings(
  rasterize(west.boreal, west.empty.raster,
            filename = file.path(rdata.path, "west_boreal.grd"),
            overwrite = TRUE)
)
save("west.boreal.raster", file = file.path(rdata.path, "west.boreal.raster.Rdata"))

sfInit(cpus = ._NUM_CPUS_., parallel = TRUE)
  sfLibrary(raster)
  sfExport("ab_pnts_boreal", "crs.boreal", "rdata.path", "west.boreal.raster")
  
  ab_pnts_boreal_stack <- sfClusterApplyLB(names(ab_pnts_boreal), function(shp) {
    pnts <- ab_pnts_boreal[[shp]]
    yr <- substr(shp, 1, 4)
    out <- rasterize(pnts, west.boreal.raster,
                     field = as.numeric(pnts@data$NUM_TREES), fun = "sum",
                     filename = file.path(rdata.path, paste0("MPB_AB_pnts_", yr, "_new.grd")),
                     overwrite = TRUE)
    writeRaster(out, filename = file.path(rdata.path, paste0("MPB_AB_pnts_", yr, "_new.tif")),
                overwrite = TRUE)
    return(out)
  }) %>%
    setNames(names(ab_pnts_boreal)) %>%
    stack(filename = file.path(rdata.path, "ab_pnts_boreal_2016.grd"), overwrite = TRUE)
  
  save("ab_pnts_boreal_stack", file = file.path(rdata.path, "ab_pnts_boreal_stack_2016.Rdata"))
sfStop()
.cleanup()
unlink(rasterOptions()$tmpfile, recursive = TRUE)

dev.new(noRStudioGD = TRUE)
plot(ab_pnts_boreal_stack)


