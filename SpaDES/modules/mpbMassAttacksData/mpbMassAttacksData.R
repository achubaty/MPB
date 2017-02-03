
# Everything in this file gets sourced during simInit, and all functions and objects
#  are put into the simList. To use objects and functions, use sim$xxx.
defineModule(sim, list(
  name = "mpbMassAttacksData",
  description = "Mountain Pine Beetle Red Top Growth Model: Short-run Potential for Establishment, Eruption, and Spread",
  keywords = c("mountain pine beetle, outbreak dynamics, eruptive potential, spread, climate change, twitch response"),
  authors = c(
    person(c("Alex", "M"), "Chubaty", email = "alexander.chubaty@canada.ca", role = c("aut", "cre")),
    person(c("Barry", "J"), "Cooke", email = "barry.cooke@ontario.ca", role = c("aut")),
    person(c("Eliot", "J B"), "McIntire", email = "eliot.mcintire@canada.ca", role = c("aut"))
  ),
  childModules = character(),
  version = numeric_version("0.0.1"),
  spatialExtent = raster::extent(rep(NA_real_, 4)),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year",
  citation = list(),
  reqdPkgs = list("data.table", "raster", "RColorBrewer"),
  parameters = rbind(
    defineParameter(".plotInitialTime", "numeric", NA, NA, NA, "This describes the simulation time at which the first plot event should occur"),
    defineParameter(".plotInterval", "numeric", 1, NA, NA, "This describes the interval between plot events"),
    defineParameter(".saveInitialTime", "numeric", NA, NA, NA, "This describes the simulation time at which the first save event should occur"),
    defineParameter(".saveInterval", "numeric", NA, NA, NA, "This describes the interval between save events"),
    defineParameter(".useCache", "numeric", FALSE, NA, NA, "Should this entire module be run with caching activated?")
  ),
  inputObjects = bind_rows(
    expectsInput("studyArea", "SpatialPolygons", "The study area to which all maps will be cropped and reprojected.", sourceURL = NA),
    expectsInput("mpbGrowthDT", "data.table", "Current MPB attack map (number of red attacked trees).")
  ),
  outputObjects = bind_rows(
    createsOutput("massAttacksDT", "data.table", "Current MPB attack map (number of red attacked trees).")
  )
))

## event types
#   - type `init` is required for initiliazation

doEvent.mpbMassAttacksData <- function(sim, eventTime, eventType, debug = FALSE) {
  switch(eventType,
    "init" = {
      ### check for more detailed object dependencies:
      ### (use `checkObject` or similar)
  
      # do stuff for this event
      sim <- sim$mpbMassAttacksDataInit(sim)
  
      # schedule future event(s)
      sim <- scheduleEvent(sim, params(sim)$mpbMassAttacksData$.plotInitialTime,
                           "mpbMassAttacksData", "plot", .last() - 1)
      sim <- scheduleEvent(sim, params(sim)$mpbMassAttacksData$.saveInitialTime,
                           "mpbMassAttacksData", "save", .last())
    },
    "plot" = {
      # ! ----- EDIT BELOW ----- ! #
      # do stuff for this event
      Plot(sim$massAttacksMap)
  
      # schedule future event(s)
      sim <- scheduleEvent(sim, time(sim) + params(sim)$mpbMassAttacksData$.plotInterval,
                           "mpbMassAttacksData", "plot", .last() - 1)
  
      # ! ----- STOP EDITING ----- ! #
    },
    warning(paste("Undefined event type: '", events(sim)[1, "eventType", with = FALSE],
                  "' in module '", events(sim)[1, "moduleName", with = FALSE], "'", sep = ""))
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
    load(file.path(modulePath(sim), "mpbMassAttacksData", "data", "west.boreal.RData"), envir = envir(sim))
  }
  
  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}

## event functions
#   - follow the naming convention `modulenameEventtype()`;
#   - `modulenameInit()` function is required for initiliazation;
#   - keep event functions short and clean, modularize by calling subroutines from section below.

### template initilization
mpbMassAttacksDataInit <- function(sim) {
  # ! ----- EDIT BELOW ----- ! #
  ##
  ## TO DO: incorporate code from MPB_maps.R to create the raster layers
  ##
  
  f <- file.path(modulePath(sim), "mpbMassAttacksData", "data", "mpb_bcab_boreal.tif")
  
  fn1 <- function(f, studyArea) {
    tf <- tempfile(fileext = ".tif")
    file.create(tf)
    a <- raster(x = f)
    b <- spTransform(studyArea, CRSobj = CRS(proj4string(a)))
    a <- crop(a, b) %>%
      projectRaster(., crs = CRS(proj4string(studyArea)), method = "ngb") %>%
      crop(studyArea)
    a[] <- a[]
    
    setColors(a) <- brewer.pal(9, "YlOrRd")
    a <- writeRaster(a, filename = tf, overwrite = TRUE)
    return(a)
  }
  
  sim$massAttacksMap <- Cache(fn1, f, sim$studyArea)

  ids <- which(!is.na(sim$massAttacksMap[]))
  sim$massAttacksDT <- data.table(ID = ids, RedTrees = sim$massAttacksMap[ids])

  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}
