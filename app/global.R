## load packages and set various options ---------------------------------------
if (tryCatch(packageVersion("amc") < "0.1.1.9003", error = function(x) TRUE)) {
  devtools::install_github("achubaty/amc@development")
}

if (tryCatch(packageVersion("SpaDES.core") < "0.1.1", error = function(x) TRUE)) {
  install.packages("SpaDES.core")
}

if (tryCatch(packageVersion("SpaDES.shiny") < "0.1.0", error = function(x) TRUE)) {
  devtools::install_github("PredictiveEcology/SpaDES.shiny@development")
}

library(shiny)
library(shinyBS)
library(shinydashboard)
library(shinyjs)

library(sp)
library(rgdal)
library(raster)
library(leaflet)

library(amc)
library(data.table)
library(DiagrammeR)
library(ggvis)
library(magrittr)
library(markdown)
library(parallel)
library(RColorBrewer)

library(quickPlot)
library(reproducible)
library(SpaDES.tools)
library(SpaDES.core)
library(SpaDES.shiny)

raster::rasterOptions(chunksize = 1e9, maxmemory = 4e10)

._MAXCLUSTERS_. <- 2
._OS_. <- tolower(Sys.info()[["sysname"]])
._USER_. <- Sys.info()[["user"]]

._POLYNUM_. <- 609  ## ecodistrict polygon number to use for demoArea

## set simulation paths
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

## check that symlinked directories exist; if not, create them
appDir <- file.path("~/GitHub/MPB/app")

links <- c(
  file.path(appDir, "cache"),
  file.path(appDir, "modules"),
  file.path(paths$inputPath, "studyArea")
)
targets <- c(
  file.path("~/SpaDES/cache"),
  file.path("~/GitHub/MPB/SpaDES/modules"),
  file.path("~/GitHub/MPB/SpaDES/studyArea")
)
stopifnot(length(links) == length(targets))

for (i in 1:length(links)) {
  if (normalizePath(Sys.readlink(links[i])) != normalizePath(targets[i])) {
    file.symlink(links[i], dirname(targets[i]))
  }
}

rm(links, targets)

## source additional app functions / modules -----------------------------------
brk <- function() {
  paste0(paste0(rep("-", getOption("width")), collapse = ""), "\n")
}

#source("footers.R")    # these functions now in SpaDES.shiny package
source("inputMaps.R")
source("leaflet.R")
#source("simInfo.R")    # these functions now in SpaDES.shiny package
#source("simOutputs.R") # these functions now in SpaDES.shiny package

## ---- begin "for development use only"
if (FALSE) {
  ## These are all "dangerous"!
  ## in the sense that they should never be run inadvertently
  ## To rerun the spades initial call, delete the mySim object in the .GlobalEnv ##
  reproducible::clearCache(cacheRepo = "cache")
  rm(cl)
  file.remove(dir("outputs", recursive = TRUE, full.names = TRUE))
  unlink("outputs", force = TRUE)
  unlink("cache", force = TRUE, recursive = TRUE)
}
## ---- end "for development use only"

copyrightInfo <- paste(
  shiny::icon("copyright",  lib = "font-awesome"), "Copyright",
  format(Sys.time(), "%Y"),
  paste("Her Majesty the Queen in Right of Canada,",
        "as represented by the Minister of Natural Resources Canada.")
)

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
    message("  Finished spawning multiple threads.")
  }
}

## initialize simulation -------------------------------------------------------

## load CRS for the boreal raster
crs.boreal <- paste("+proj=aea +lat_1=47.5 +lat_2=54.5 +lat_0=0 +lon_0=-113 +x_0=0 +y_0=0",
                    "+datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0")

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

