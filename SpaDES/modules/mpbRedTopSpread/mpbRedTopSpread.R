
# Everything in this file gets sourced during simInit, and all functions and objects
#  are put into the simList. To use objects and functions, use sim$xxx.
defineModule(sim, list(
  name = "mpbRedTopSpread",
  description = "Mountain Pine Beetle Red Top Growth Model: Short-run Potential for Establishment, Eruption, and Spread",
  keywords = c("mountain pine beetle, outbreak dynamics, eruptive potential, spread, climate change, twitch response"),
  authors = c(
    person(c("Alex", "M"), "Chubaty", email = "alexander.chubaty@canada.ca", role = c("aut", "cre")),
    person(c("Barry", "J"), "Cooke", email = "barry.cooke@ontario.ca", role = c("aut"))
  ),
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
    defineParameter(".useCache", "numeric", FALSE, NA, NA, "Should this entire module be run with caching activated?"),
    defineParameter("dispersalInterval", "numeric", 1, NA, NA, "This describes the interval time between dispersal events")
  ),
  inputObjects = bind_rows(
    expectsInput("climateSuitabilityMap", "RasterLayer", "A climatic suitablity map for the current year."),
    expectsInput("pineDT", "data.table", "Current lodgepole and jack pine available for MPB."),
    expectsInput("massAttacksDT", "data.table", "Current MPB attack map (number of red attacked trees)."),
    expectsInput("mpbGrowthDT", "data.table", "Current MPB attack map (number of red attacked trees).")
  ),
  outputObjects = bind_rows(
    createsOutput("mpbGrowthDT", "data.table", "Current MPB attack map (number of red attacked trees).")
  )
))

## event types
#   - type `init` is required for initiliazation

doEvent.mpbRedTopSpread <- function(sim, eventTime, eventType, debug = FALSE) {
  switch(eventType,
    "init" = {
      ### check for more detailed object dependencies:
      ### (use `checkObject` or similar)
  
      # do stuff for this event
      sim <- sim$mpbRedTopSpreadInit(sim)
  
      # schedule future event(s)
      sim <- scheduleEvent(sim, time(sim) + params(sim)$mpbRedTopSpread$dispersalInterval,
                           "mpbRedTopSpread", "dispersal")
      sim <- scheduleEvent(sim, params(sim)$mpbRedTopSpread$.plotInitialTime, "mpbRedTopSpread", "plot")
      sim <- scheduleEvent(sim, params(sim)$mpbRedTopSpread$.saveInitialTime, "mpbRedTopSpread", "save")
    },
    "dispersal" = {
      # ! ----- EDIT BELOW ----- ! #
      # do stuff for this event
  
      sim <- sim$mpbRedTopSpreadDispersal(sim)
  
      sim <- scheduleEvent(sim, time(sim) + params(sim)$mpbRedTopSpread$dispersalInterval,
                           "mpbRedTopSpread", "dispersal")
  
      # ! ----- STOP EDITING ----- ! #
    },
    "plot" = {
      # ! ----- EDIT BELOW ----- ! #
      # do stuff for this event
  
      # e.g., call your custom functions/methods here
      # you can define your own methods below this `doEvent` function
  
      # schedule future event(s)
  
      # e.g.,
      # sim <- scheduleEvent(sim, time(sim) + increment, "mpbRedTopSpread", "plot")
  
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
    load(file.path(modulePath(sim), "mpbRedTopSpread", "data", "west.boreal.RData"), envir = envir(sim))
  }
  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}

## event functions
#   - follow the naming convention `modulenameEventtype()`;
#   - `modulenameInit()` function is required for initiliazation;
#   - keep event functions short and clean, modularize by calling subroutines from section below.

### template initilization
mpbRedTopSpreadInit <- function(sim) {

  # # ! ----- EDIT BELOW ----- ! #



  # ! ----- STOP EDITING ----- ! #

  return(invisible(sim))
}

### template for plot events
mpbRedTopSpreadPlot <- function(sim) {
  # ! ----- EDIT BELOW ----- ! #
  # do stuff for this event
  #Plot("object")

  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}

### template for your event1
mpbRedTopSpreadDispersal <- function(sim) {
  # ! ----- EDIT BELOW ----- ! #

  sim$totalNumberMassAttacks <- sim$totalNumberMassAttacks # doesn't do anything!

  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}


### add additional events as needed by copy/pasting from above

