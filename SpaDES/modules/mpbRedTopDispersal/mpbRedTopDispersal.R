
# Everything in this file gets sourced during simInit, and all functions and objects
#  are put into the simList. To use objects and functions, use sim$xxx.
defineModule(sim, list(
  name="MPBRedTopDispersal_Null",
  description="Mountain Pine Beetle Red Top Growth Model: Short-run Potential for Establishment, Eruption, and Spread",
  keywords=c("mountain pine beetle, outbreak dynamics, eruptive potential, spread, climate change, twitch response"),
  authors=c(person(c("Barry", "J"), "Cooke", email="Barry.Cooke@NRCan.gc.ca", role=c("aut", "cre")),
            person(c("Alex", "M"), "Chubaty", email="Alexander.Chubaty@NRCan.gc.ca", role=c("aut", "cre"))),
  childModules=character(),
  version=numeric_version("0.0.1"),
  spatialExtent=raster::extent(rep(NA_real_, 4)),
  timeframe=as.POSIXlt(c(NA, NA)),
  timeunit="year",
  citation=list(),
  reqdPkgs=list("raster", "RColorBrewer"),
  parameters=rbind(
    defineParameter(".plotInitialTime", "numeric", NA, NA, NA, "This describes the simulation time at which the first plot event should occur"),
    defineParameter(".plotInterval", "numeric", 1, NA, NA, "This describes the interval between plot events"),
    defineParameter(".saveInitialTime", "numeric", NA, NA, NA, "This describes the simulation time at which the first save event should occur"),
    defineParameter(".saveInterval", "numeric", NA, NA, NA, "This describes the interval between save events"),
    defineParameter(".useCache", "numeric", FALSE, NA, NA, "Should this entire module be run with caching activated? This is generally intended for data-type modules, where stochasticity and time are not relevant"),
    defineParameter("dispersalInterval", "numeric", 1, NA, NA, "This describes the interval time between dispersal events")
  ),
  inputObjects=data.frame(objectName=c(globals(sim)$stackName),
                          objectClass=c("RasterStack"),
                          other=c(NA_character_),
                          stringsAsFactors=FALSE),
  outputObjects=data.frame(objectName=NA_character_, objectClass=NA_character_, other=NA_character_, stringsAsFactors=FALSE)
))

## event types
#   - type `init` is required for initiliazation

doEvent.MPBRedTopDispersal_Null = function(sim, eventTime, eventType, debug=FALSE) {
  if (eventType=="init") {
    ### check for more detailed object dependencies:
    ### (use `checkObject` or similar)

    # do stuff for this event
    sim <- sim$MPBRedTopDispersal_NullInit(sim)

    # schedule future event(s)
    sim <- scheduleEvent(sim, time(sim)+params(sim)$MPBRedTopDispersal_Null$dispersalInterval, "MPBRedTopDispersal_Null", "dispersal")
    sim <- scheduleEvent(sim, start(sim)+params(sim)$MPBRedTopDispersal_Null$.plotInitialTime, "MPBRedTopDispersal_Null", "plot")
    sim <- scheduleEvent(sim, start(sim)+params(sim)$MPBRedTopDispersal_Null$.saveInitialTime, "MPBRedTopDispersal_Null", "save")
  } else if (eventType=="dispersal") {
    # ! ----- EDIT BELOW ----- ! #
    # do stuff for this event

    sim <- sim$MPBRedTopDispersal_NullDispersal(sim)

    # e.g., call your custom functions/methods here
    # you can define your own methods below this `doEvent` function

    # schedule future event(s)

    # e.g.,
    # sim <- scheduleEvent(sim, time(sim) + increment, "MPBRedTopDispersal_Null", "save")

    # ! ----- STOP EDITING ----- ! #
  } else if (eventType=="plot") {
    # ! ----- EDIT BELOW ----- ! #
    # do stuff for this event

    # e.g., call your custom functions/methods here
    # you can define your own methods below this `doEvent` function

    # schedule future event(s)

    # e.g.,
    # sim <- scheduleEvent(sim, time(sim) + increment, "MPBRedTopDispersal_Null", "plot")

    # ! ----- STOP EDITING ----- ! #
  } else if (eventType=="save") {
    # ! ----- EDIT BELOW ----- ! #
    # do stuff for this event

    # e.g., call your custom functions/methods here
    # you can define your own methods below this `doEvent` function

    # schedule future event(s)

    # e.g.,
    # sim <- scheduleEvent(sim, time(sim) + increment, "MPBRedTopDispersal_Null", "save")

    # ! ----- STOP EDITING ----- ! #
  } else {
      warning(paste("Undefined event type: '", events(sim)[1, "eventType", with=FALSE],
                    "' in module '", events(sim)[1, "moduleName", with=FALSE], "'", sep=""))
  }
  return(invisible(sim))
}

## event functions
#   - follow the naming convention `modulenameEventtype()`;
#   - `modulenameInit()` function is required for initiliazation;
#   - keep event functions short and clean, modularize by calling subroutines from section below.

### template initilization
MPBRedTopDispersal_NullInit <- function(sim) {

  # # ! ----- EDIT BELOW ----- ! #



  # ! ----- STOP EDITING ----- ! #

  return(invisible(sim))
}

### template for save events
MPBRedTopDispersal_NullSave <- function(sim) {
  # ! ----- EDIT BELOW ----- ! #
  # do stuff for this event
  sim <- saveFiles(sim)

  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}

### template for plot events
MPBRedTopDispersal_NullPlot <- function(sim) {
  # ! ----- EDIT BELOW ----- ! #
  # do stuff for this event
  #Plot("object")

  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}

### template for your event1
MPBRedTopDispersal_NullDispersal <- function(sim) {
  # ! ----- EDIT BELOW ----- ! #

  sim$totalNumberMassAttacks <- sim$totalNumberMassAttacks # doesn't do anything!

  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}


### add additional events as needed by copy/pasting from above

