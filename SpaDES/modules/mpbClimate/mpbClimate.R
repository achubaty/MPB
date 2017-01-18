
# Everything in this file gets sourced during simInit, and all functions and objects
# are put into the simList. To use objects and functions, use sim$xxx.
defineModule(sim, list(
  name = "mpbClimate",
  description = "insert module description here",
  keywords = c("insert key words here"),
  authors = c(person(c("Alex", "M"), "Chubaty", email = "alexander.chubaty@canada.ca", role = c("aut", "cre"))),
  childModules = character(0),
  version = numeric_version("1.3.1.9020"),
  spatialExtent = raster::extent(rep(NA_real_, 4)),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year",
  citation = list("citation.bib"),
  documentation = list("README.txt", "mpbClimate.Rmd"),
  reqdPkgs = list("magrittr", "raster", "sp"),
  parameters = rbind(
    defineParameter("climateScenario", "character", "RCP45", NA_character_, NA_character_, "The climate scenario to use. One of RCP45 or RCP85."),
    defineParameter("suitabilityIndex", "character", "G", NA_character_, NA_character_, "The MPB climatic suitabilty index to use. One of S, L, R, or G."),
    defineParameter(".plotInitialTime", "numeric", start(sim), 1981, 2100, "This describes the simulation time at which the first plot event should occur"),
    defineParameter(".plotInterval", "numeric", NA, NA, NA, "This describes the simulation time interval between plot events"),
    defineParameter(".saveInitialTime", "numeric", NA, NA, NA, "This describes the simulation time at which the first save event should occur"),
    defineParameter(".saveInterval", "numeric", NA, NA, NA, "This describes the simulation time interval between save events"),
    defineParameter(".useCache", "numeric", FALSE, NA, NA, "Should this entire module be run with caching activated? This is generally intended for data-type modules, where stochasticity and time are not relevant")
  ),
  inputObjects = bind_rows(
    expectsInput(objectName = "studyArea", objectClass = "SpatialPolygons",
                 desc = "The study area to which all maps will be cropped and reprojected.", sourceURL = NA)
  ),
  outputObjects = bind_rows(
    createsOutput(objectName = "climateSuitabilityMap", objectClass = "RasterLayer",
                  desc = "A stack of MPB climatic suitablity maps corresponding to 1981-2010 normals and 2011-2040, 2041-2070, 2071-2100 projections.")
  )
))

## event types
#   - type `init` is required for initialiazation

doEvent.mpbClimate = function(sim, eventTime, eventType, debug = FALSE) {
  if (eventType == "init") {
    ### check sim init params etc.
    stopifnot(start(sim) > 1981, end(sim) < 2100)

    # do stuff for this event
    sim <- sim$mpbClimateInit(sim)
    sim <- sim$mpbClimateImportMaps(sim)

    # schedule future event(s)
    sim <- scheduleEvent(sim, start(sim), "mpbClimate", "switchLayer", .first())
    sim <- scheduleEvent(sim, P(sim)$.plotInitialTime, "mpbClimate", "plot", .last())
    sim <- scheduleEvent(sim, P(sim)$.saveInitialTime, "mpbClimate", "save", .last() + 1)
  } else if (eventType == "plot") {
    # ! ----- EDIT BELOW ----- ! #
    # do stuff for this event
    names(sim$climateSuitabilityMap) <- "layer"
    Plot(sim$climateSuitabilityMap, title = "Climate Suitability Map", new = TRUE)
    
    # schedule future event(s)

    sim <- scheduleEvent(sim, time(sim) + P(sim)$.plotInterval, "mpbClimate", "plot")
    # ! ----- STOP EDITING ----- ! #
  } else if (eventType == "save") {
    # ! ----- EDIT BELOW ----- ! #
    # do stuff for this event

    # e.g., call your custom functions/methods here
    # you can define your own methods below this `doEvent` function

    # schedule future event(s)

    # e.g.,
    # sim <- scheduleEvent(sim, time(sim) + increment, "mpbClimate", "save")

    # ! ----- STOP EDITING ----- ! #
  } else if (eventType == "switchLayer") {
    sim <- mpbClimateSwitchLayer(sim)
    
    sim <- scheduleEvent(sim, time(sim) + 40, "mpbClimate", "switchLayer")
  } else {
    warning(paste("Undefined event type: '", current(sim)[1, "eventType", with = FALSE],
                  "' in module '", current(sim)[1, "moduleName", with = FALSE], "'", sep = ""))
  }
  return(invisible(sim))
}

## event functions
#   - follow the naming convention `modulenameEventtype()`;
#   - `modulenameInit()` function is required for initiliazation;
#   - keep event functions short and clean, modularize by calling subroutines from section below.

### template initialization
mpbClimateInit <- function(sim) {
  return(invisible(sim))
}

### template for save events
mpbClimateSave <- function(sim) {
  # ! ----- EDIT BELOW ----- ! #
  # do stuff for this event
  sim <- saveFiles(sim)

  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}

### template for plot events
mpbClimatePlot <- function(sim) {
  Plot(sim$mpbClimateMap)
  return(invisible(sim))
}

### template for your event2
mpbClimateSwitchLayer <- function(sim) {
  # ! ----- EDIT BELOW ----- ! #
  sim$climateSuitabilityMap <- if (time(sim) <= 2010) {
    sim$mpbClimateMaps[[1]]
  } else if (time(sim) <= 2040) {
    sim$mpbClimateMaps[[2]]
  } else if (time(sim) <= 2070) {
    sim$mpbClimateMaps[[3]]
  } else if (time(sim) <= 2100) {
    sim$mpbClimateMaps[[4]]
  } else {
    stop("No climate suitabliity projections available beyond year 2100.")
  }

  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}

.inputObjects <- function(sim) {
  # Any code written here will be run during the simInit for the purpose of creating
  # any objects required by this module and identified in the inputObjects element of defineModule.
  # This is useful if there is something required before simulation to produce the module
  # object dependencies, including such things as downloading default datasets, e.g.,
  # downloadData("LCC2005", modulePath(sim)).
  # Nothing should be created here that does not create an named object in inputObjects.
  # Any other initiation procedures should be put in "init" eventType of the doEvent function.
  # Note: the module developer can use 'sim$.userSuppliedObjNames' in their function below to
  # selectively skip unnecessary steps because the user has provided those inputObjects in the
  # simInit call. e.g.,
  # if (!('defaultColor' %in% sim$.userSuppliedObjNames)) {
  #  sim$defaultColor <- 'red'
  # }
  # ! ----- EDIT BELOW ----- ! #
  if (!('studyArea' %in% sim$.userSuppliedObjNames)) {
    load(file.path(modulePath(sim), "mpbClimate", "data", "west.boreal.RData"), envir = envir(sim))
  }
  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}

### helper functions
mpbClimateImportMaps <- function(sim) {
  suffix <- switch(P(sim)$suitabilityIndex,
                   "S" = "_SafP[.]tif",
                   "L" = "_LoganP[.]tif",
                   "R" = "_RegP[.]tif",
                   "G" = "_GeoP[.]tif",
                   stop("suitability index must be one of S, L, R, or G."))
  files <- dir(path = file.path(modulePath(sim), "mpbClimate", "data"), pattern = suffix, full.names = TRUE)
  files <- c(files[1], grep(P(sim)$climateScenario, files, value = TRUE))
  
  fn1 <- function(files, studyArea) {
    stack(lapply(files, function(f) {
      raster(f) %>%
        crop(studyArea) %>%
        projectRaster(crs = CRS(proj4string(studyArea)))
    }))
  }
  sim$mpbClimateMaps <- Cache(fn1, files, sim$studyArea)
  
  return(sim)
}
