
# Everything in this file gets sourced during simInit, and all functions and objects
#  are put into the simList. To use objects and functions, use sim$xxx.
defineModule(sim, list(
  name = "mpbMassAttacks",
  description = "Mountain Pine Beetle Red Top Growth Model: Short-run Potential for Establishment, Eruption, and Spread",
  keywords = c("mountain pine beetle, outbreak dynamics, eruptive potential, spread, climate change, twitch response"),
  authors = c(person(c("Barry", "J"), "Cooke", email = "barry.cooke@ontario.ca", role = c("aut", "cre")),
              person(c("Alex", "M"), "Chubaty", email = "alexander.chubaty@canada.ca", role = c("aut", "cre"))),
  childModules = character(),
  version = numeric_version("0.0.1"),
  spatialExtent = raster::extent(rep(NA_real_, 4)),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year",
  citation = list(),
  reqdPkgs = list("raster", "RColorBrewer"),
  parameters = rbind(
    defineParameter(".plotInitialTime", "numeric", NA, NA, NA, "This describes the simulation time at which the first plot event should occur"),
    defineParameter(".plotInterval", "numeric", 1, NA, NA, "This describes the interval between plot events"),
    defineParameter(".saveInitialTime", "numeric", NA, NA, NA, "This describes the simulation time at which the first save event should occur"),
    defineParameter(".saveInterval", "numeric", NA, NA, NA, "This describes the interval between save events"),
    defineParameter(".useCache", "numeric", FALSE, NA, NA, "Should this entire module be run with caching activated?")
  ),
  inputObjects = bind_rows(
    expectsInput(objectName = "studyArea", objectClass = "SpatialPolygons",
                 desc = "The study area to which all maps will be cropped and reprojected.", sourceURL = NA)
  ),
  outputObjects = bind_rows(
    createsOutput("massAttacksT", "RasterLayer", desc = "The current year's mass attacks."),
    createsOutput("massAttacksTminus1", "RasterLayer", desc = "The previous year's mass attacks.")
  )
))

## event types
#   - type `init` is required for initiliazation

doEvent.mpbMassAttacks <- function(sim, eventTime, eventType, debug = FALSE) {
  switch(eventType,
    "init" = {
      ### check for more detailed object dependencies:
      ### (use `checkObject` or similar)
  
      # do stuff for this event
      sim <- sim$mpbMassAttacksInit(sim)
  
      # schedule future event(s)
      sim <- scheduleEvent(sim, start(sim) + params(sim)$mpbMassAttacks$.plotInitialTime,
                           "mpbMassAttacks", "plot")
      sim <- scheduleEvent(sim, start(sim) + params(sim)$mpbMassAttacks$.saveInitialTime,
                           "mpbMassAttacks", "save")
    },
    "plot" = {
      # ! ----- EDIT BELOW ----- ! #
      # do stuff for this event
      Plot(sim$MassAttacksT)
  
      # schedule future event(s)
      sim <- scheduleEvent(sim, time(sim) + params(sim)$mpbMassAttacks$.plotInterval,
                           "mpbMassAttacks", "plot")
  
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
    load(file.path(modulePath(sim), "mpbMassAttacks", "data", "west.boreal.RData"), envir = envir(sim))
  }
  
  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}

## event functions
#   - follow the naming convention `modulenameEventtype()`;
#   - `modulenameInit()` function is required for initiliazation;
#   - keep event functions short and clean, modularize by calling subroutines from section below.

### template initilization
mpbMassAttacksInit <- function(sim) {
  # ! ----- EDIT BELOW ----- ! #
  ##
  ## TO DO: incorporate code from MPB_maps.R to create the raster layers
  ##
  massAttacks <- stack(file.path(modulePath(sim), "mpbMassAttacks", "data", "mpb_bcab_boreal.tif")) %>% 
    crop(sim$studyArea)
  setColors(massAttacks) <- brewer.pal(9, "YlOrRd") ## does this work on a stack?
  sim$MassAttacksT <- massAttacks[[time(sim)]]
  sim$MassAttacksTminus1 <- massAttacks[[time(sim) - 1]]

  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}
