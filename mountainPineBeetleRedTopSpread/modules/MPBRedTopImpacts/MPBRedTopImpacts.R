
# Everything in this file gets sourced during simInit, and all functions and objects
#  are put into the simList. To use objects and functions, use sim$xxx.
defineModule(sim, list(
  name="MPBRedTopImpacts",
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
    defineParameter("impactsInterval", "numeric", 1, NA, NA, "This describes the interval time between impact events")
  ),
  inputObjects=data.frame(objectName=c(globals(sim)$stackName, "massAttacksT"),
                          objectClass=c("RasterStack", "RasterLayer"),
                          other=c(NA_character_, NA_character_),
                          stringsAsFactors=FALSE),
  outputObjects=data.frame(objectName=c(globals(sim)$stackName),
                           objectClass=c("RasterStack"),
                           other=c(NA_character_),
                           stringsAsFactors=FALSE)
))

## event types
#   - type `init` is required for initiliazation

doEvent.MPBRedTopImpacts = function(sim, eventTime, eventType, debug=FALSE) {
  if (eventType=="init") {
    ### check for more detailed object dependencies:
    ### (use `checkObject` or similar)

    # do stuff for this event
    sim <- sim$MPBRedTopImpactsInit(sim)

    # schedule future event(s)
    sim <- scheduleEvent(sim, start(sim)+params(sim)$MPBRedTopImpacts$.plotInitialTime, "MPBRedTopImpacts", "plot")
    sim <- scheduleEvent(sim, start(sim)+params(sim)$MPBRedTopImpacts$.saveInitialTime, "MPBRedTopImpacts", "save")
  } else if (eventType=="plot") {
    # ! ----- EDIT BELOW ----- ! #
    # do stuff for this event
    #Plot(something)

    # schedule future event(s)

    # e.g.,
    # sim <- scheduleEvent(sim, time(sim) + increment, "MPBRedTopImpacts", "plot")

    # ! ----- STOP EDITING ----- ! #
  } else if (eventType=="save") {
    # ! ----- EDIT BELOW ----- ! #
    # do stuff for this event

    # e.g., call your custom functions/methods here
    # you can define your own methods below this `doEvent` function

    # schedule future event(s)

    # e.g.,
    # sim <- scheduleEvent(sim, time(sim) + increment, "MPBRedTopImpacts", "save")

    # ! ----- STOP EDITING ----- ! #
  } else if (eventType=="event1") {
    # ! ----- EDIT BELOW ----- ! #
    # do stuff for this event

    # e.g., call your custom functions/methods here
    # you can define your own methods below this `doEvent` function

    # schedule future event(s)

    # e.g.,
    # sim <- scheduleEvent(sim, time(sim) + increment, "MPBRedTopImpacts", "templateEvent")

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
MPBRedTopImpactsInit <- function(sim) {

  # # ! ----- EDIT BELOW ----- ! #



  # ! ----- STOP EDITING ----- ! #

  return(invisible(sim))
}

### template for save events
MPBRedTopImpactsSave <- function(sim) {
  # ! ----- EDIT BELOW ----- ! #
  # do stuff for this event
  sim <- saveFiles(sim)

  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}

### template for your event1
MPBRedTopImpactsEvent1 <- function(sim) {
  # ! ----- EDIT BELOW ----- ! #



  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}

### add additional events as needed by copy/pasting from above

