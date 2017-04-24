
# Everything in this file gets sourced during simInit, and all functions and objects
# are put into the simList. To use objects and functions, use sim$xxx.
defineModule(sim, list(
  name = "mpbPine",
  description = "insert module description here",
  keywords = c("insert key words here"),
  authors = c(person(c("Alex", "M"), "Chubaty", email = "alexander.chubaty@canada.ca", role = c("aut", "cre"))),
  childModules = character(0),
  version = numeric_version("0.0.1"),
  spatialExtent = raster::extent(rep(NA_real_, 4)),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year",
  citation = list("citation.bib"),
  documentation = list("README.txt", "mpbPine.Rmd"),
  reqdPkgs = list("data.table", "magrittr", "raster", "sp"),
  parameters = rbind(
    defineParameter(".plotInitialTime", "numeric", start(sim), NA, NA, "This describes the simulation time at which the first plot event should occur"),
    defineParameter(".plotInterval", "numeric", NA, NA, NA, "This describes the simulation time interval between plot events"),
    defineParameter(".saveInitialTime", "numeric", NA, NA, NA, "This describes the simulation time at which the first save event should occur"),
    defineParameter(".saveInterval", "numeric", NA, NA, NA, "This describes the simulation time interval between save events"),
    defineParameter(".useCache", "numeric", FALSE, NA, NA, "Should this entire module be run with caching activated?"),
    defineParameter("lowMemory", "numeric", FALSE, NA, NA, "Should high memory-usage steps be skipped? Useful for running on laptops.")
  ),
  inputObjects = bind_rows(
    expectsInput("studyArea", "SpatialPolygons", "The study area to which all maps will be cropped and reprojected.", sourceURL = NA),
    expectsInput("mpbGrowthDT", "data.table", "Current MPB attack map (number of red attacked trees).")
  ),
  outputObjects = bind_rows(
    createsOutput("pineMap", "RasterLayer", "Current lodgepole and jack pine available for MPB.")
  )
))

## event types
#   - type `init` is required for initialiazation

doEvent.mpbPine <- function(sim, eventTime, eventType, debug = FALSE) {
  switch(eventType,
    "init" = {
      # do stuff for this event
      sim <- sim$mpbPineImportMap(sim)
  
      # schedule future event(s)
      sim <- scheduleEvent(sim, P(sim)$.plotInitialTime, "mpbPine", "plot")
      sim <- scheduleEvent(sim, P(sim)$.saveInitialTime, "mpbPine", "save")
    },
    "plot" = {
      # ! ----- EDIT BELOW ----- ! #
      # do stuff for this event
      Plot(sim$pineMap, title = "Lodgepole and Jack Pine")
      Plot(MPB$studyArea, addTo = "sim$pineMap")
      
      # schedule future event(s)
      sim <- scheduleEvent(sim, time(sim) + P(sim)$.plotInterval, "mpbPine", "plot")
      # ! ----- STOP EDITING ----- ! #
    },
    warning(paste("Undefined event type: '", current(sim)[1, "eventType", with = FALSE],
                  "' in module '", current(sim)[1, "moduleName", with = FALSE], "'", sep = ""))
  )
  return(invisible(sim))
}

.inputObjects <- function(sim) {
  # ! ----- EDIT BELOW ----- ! #
  if (!('studyArea' %in% sim$.userSuppliedObjNames)) {
    load(file.path(modulePath(sim), "mpbPine", "data", "west.boreal.RData"), envir = envir(sim))
  }
  
  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}

## event functions
#   - follow the naming convention `modulenameEventtype()`;
#   - `modulenameInit()` function is required for initiliazation;
#   - keep event functions short and clean, modularize by calling subroutines from section below.

mpbPineImportMap <- function(sim) {
  layerName <- "Lodgepole_and_Jack_Pine"
  
  if (P(sim)$lowMemory) {
    ## load the pre-computed raster instead of doing RAM-intensive GIS
    f <- file.path(modulePath(sim), "mpbPine", "data", "kNN_pine_map.tif")
    
    stopifnot(file.exists(f))
    
    sim$pineMap <- Cache(amc::cropReproj, f, sim$studyArea, layerName, inRAM = TRUE)
  } else {
    f <- file.path(modulePath(sim), "mpbPine", "data",
                   c("NFI_MODIS250m_kNN_Species_Pinu_Ban_v0.tif",
                     "NFI_MODIS250m_kNN_Species_Pinu_Con_v0.tif"))
    
    stopifnot(all(file.exists(f)))
    
    s <- stack(f)

    tmp <- Cache(amc::cropReproj, x = s, studyArea = sim$studyArea,
                 layerNames = c("Jack_Pine", "Lodgepole_Pine"), inRAM = TRUE)
    sim$pineMap <- Cache(amc::mosaic2, x = tmp[[1]], y = tmp[[2]], fun = sum,
                         layerName = layerName)
    rm(tmp)
  }

  return(invisible(sim))
}
