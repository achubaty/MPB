
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
    defineParameter(".useCache", "numeric", FALSE, NA, NA, "Should this entire module be run with caching activated?")
  ),
  inputObjects = bind_rows(
    expectsInput("studyArea", "SpatialPolygons", "The study area to which all maps will be cropped and reprojected.", sourceURL = NA),
    expectsInput("mpbGrowthDT", "data.table", "Current MPB attack map (number of red attacked trees).")
  ),
  outputObjects = bind_rows(
    createsOutput("pineMap", "RasterLayer", "Current lodgepole and jack pine available for MPB."),
    createsOutput("pineDT", "data.table", "Current lodgepole and jack pine available for MPB.")
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
  f <- file.path(modulePath(sim), "mpbPine", "data",
                     c("NFI_MODIS250m_kNN_Species_Pinu_Ban_v0.tif",
                       "NFI_MODIS250m_kNN_Species_Pinu_Con_v0.tif"))

  fn1 <- function(f, studyArea) {
    tf <- tempfile(fileext = ".tif")
    file.create(tf)
    ## TO DO: make this parallel -- one thread per map layer
    a <- raster::stack(x = f) %>% setNames(c("Jack_Pine", "Lodgepole_Pine"))
    b <- spTransform(studyArea, CRSobj = CRS(proj4string(a)))
    a <- crop(a, b) %>%
      projectRaster(., crs = CRS(proj4string(studyArea)), method = "ngb") %>%
      crop(studyArea)
    a[] <- a[]
    a <- writeRaster(raster::stack(a), filename = tf, overwrite = TRUE)
    
    ## TO DO: can this part be made parallel?
    out <- mosaic(a[[1]], a[[2]], fun = sum) %>% 
      setNames("Lodgepole_and_Jack_Pine") %>% 
      writeRaster(filename = tf, overwrite = TRUE)
    return(out)
  }
  sim$pineMap <- Cache(fn1, f, sim$studyArea)

  ## put all cells with pine into the data.table
  ids <- which(sim$pineMap > 0)
  sim$pineDT <- data.table(ID = ids, pPine = rep(TRUE, length(ids))) 
  
  return(invisible(sim))
}
