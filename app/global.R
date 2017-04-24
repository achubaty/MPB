## load packages and set various options ---------------------------------------
if (tryCatch(packageVersion("amc") < "0.1.1.9000", error = function(x) TRUE)) {
  devtools::install_github("achubaty/amc")
}

if (tryCatch(packageVersion("SpaDES") < "1.3.1.9066", error = function(x) TRUE)) {
  devtools::install_github("PredictiveEcology/SpaDES@development")
}

library(shiny)
library(shinyBS)
library(shinydashboard)
library(shinyjs)

library(sp)
library(rgdal)
library(raster)
library(SpaDES)
library(leaflet)

library(amc)
library(data.table)
library(DiagrammeR)
library(ggvis)
library(magrittr)
library(markdown)
library(parallel)

raster::rasterOptions(chunksize = 1e9, maxmemory = 4e10)

._MAXCLUSTERS_. <- 2
._OS_. <- tolower(Sys.info()[["sysname"]])
._USER_. <- Sys.info()[["user"]]

paths <- list(
  cachePath = "cache",    ## symlinked to ~/SpaDES/cache
  modulePath = "modules", ## symlinked to ~/GitHub/MPB/SpaDES/modules
  inputPath = "inputs",
  outputPath = "outputs"
)
setPaths(
  cachePath = paths$cachePath,
  modulePath = paths$modulePath,
  inputPath = paths$inputPath,
  outputPath = paths$outputPath
)

## source additional app functions / modules -----------------------------------
brk <- function() {
  paste0(paste0(rep("-", getOption("width")), collapse = ""), "\n")
}

source("footers.R")
source("inputMaps.R")
source("leaflet.R")
source("simInfo.R")

## ---- begin "for development use only"
if (FALSE) {
  ## These are all "dangerous"!
  ## in the sense that they should never be run inadvertently
  ## To rerun the spades initial call, delete the mySim object in the .GlobalEnv ##
  SpaDES::clearCache(cacheRepo = "cache")
  rm(cl)
  file.remove(dir("outputs", recursive = TRUE, full.names = TRUE))
  unlink("outputs", force = TRUE)
  unlink("cache", force = TRUE, recursive = TRUE)
}
## ---- end "for development use only"

## initialize app --------------------------------------------------------------
message(brk(), "  initializing app [", Sys.time(), "]", "\n", brk())

if (!exists("globalRasters")) globalRasters <- list() ## what's this for?

curDir <- getwd()
message("Current working directory: ", curDir)

## start cluster if desired
if (._MAXCLUSTERS_. > 0) {
  if (!exists("cl")) {
    ncores <- if (._USER_. == "achubaty") {
      pmin(._MAXCLUSTERS_., detectCores() / 2)
    } else {
      pmin(._MAXCLUSTERS_., detectCores() - 1)
    }

    message("Spawning ", ncores, " threads")
    if (._OS_. == "windows") {
      clusterType = "SOCK"
    } else {
      clusterType = "FORK"
    }
    cl <- makeCluster(ncores, type = clusterType)
    if (._OS_. == "windows") {
      #clusterExport(cl = cl, varlist = list("objects", "shpStudyRegion"))
    }
    message("  Finished Spawning multiple threads.")
  }
}

## initialize simulation -------------------------------------------------------

## load CRS for the boreal raster
load(file.path(getOption("spades.modulePath"), "mpbRandomLandscapes", "data",
               "west.boreal.RData")) ## loads `studyArea` object (spdf)
crs.boreal <- CRS(proj4string(studyArea))

times <- list(start = 2011, end = 2020)
parameters <- list(
  mpbClimateData = list(suitabilityIndex = "G"),    ## Can be "G", "S", "L", "R"
  mpbPine = list(lowMemory = TRUE), ## set to TRUE on laptop
  mpbRedTopGrowth = list(dataset = "Berryman1979_forced")
)

objects <- list(studyArea = demoArea) ## demoArea defined in inputMaps.R

modules <- list(
  "mpbPine", "mpbClimateData",
  "mpbMassAttacksData",
  "mpbRedTopGrowth",
  "mpbRedTopSpread",
  "mpbManagement"
)

# outputs <- data.frame(
#   stringsAsFactors = FALSE,
#   expand.grid(
#     objectName = objectNamesToSave,#, "oldBigPatch"),
#     saveTime = seq(objects$summaryPeriod[1], objects$summaryPeriod[2],
#                    by = parameters$LandWebOutput$summaryInterval)
#   ),
#   fun = "writeRaster", package = "raster",
#   file = paste0(objectNamesToSave, c(".tif", ".grd"))
# )
#
# outputs2 <- data.frame(
#   stringsAsFactors = FALSE,
#   expand.grid(objectName = c("simulationOutput"), saveTime = times$end),
#   fun = "saveRDS", package = "base"
# )
#
# outputs$arguments <- I(rep(
#   list(
#     list(overwrite = TRUE, progress = FALSE, datatype = "INT2U", format = "GTiff"),
#     list(overwrite = TRUE, progress = FALSE, datatype = "INT1U", format = "raster")
#   ),
#   times = NROW(outputs) / length(objectNamesToSave)
# ))
#
# outputs <- as.data.frame(rbindlist(list(outputs, outputs2), fill = TRUE))

mySim <- simInit(times = times, params = parameters, modules = modules,
                 objects = objects, paths = paths)#, outputs = outputs)

## -----------------------------------------------------------------------------
message(brk(), "finished running global.R [", Sys.time(), "]", "\n", brk())
