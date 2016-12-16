
# Everything in this file gets sourced during simInit, and all functions and objects
#  are put into the simList. To use objects and functions, use sim$xxx.
defineModule(sim, list(
  name="MPBRedTopClimate",
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
  reqdPkgs=list("RandomFields", "raster", "RColorBrewer"),
  parameters=rbind(
    defineParameter(".plotInitialTime", "numeric", NA, NA, NA, "This describes the simulation time at which the first plot event should occur"),
    defineParameter(".plotInterval", "numeric", 1, NA, NA, "This describes the interval between plot events"),
    defineParameter(".saveInitialTime", "numeric", NA, NA, NA, "This describes the simulation time at which the first save event should occur"),
    defineParameter(".saveInterval", "numeric", NA, NA, NA, "This describes the interval between save events"),
    defineParameter("updateInterval", "numeric", 1, NA, NA, "This describes the interval time between climate layer update events")
  ),
  inputObjects=data.frame(objectName=c(globals(sim)$stackName),
                          objectClass=c("RasterStack"),
                          other=c(NA_character_),
                          stringsAsFactors=FALSE),
  outputObjects=data.frame(objectName=c(globals(sim)$stackName, globals(sim)$climateMap),
                           objectClass=c("RasterStack", "RasterStack"),
                           other=c(NA_character_, NA_character_),
                           stringsAsFactors=FALSE)
))

## event types
#   - type `init` is required for initiliazation

doEvent.MPBRedTopClimate = function(sim, eventTime, eventType, debug=FALSE) {
  if (eventType=="init") {
    ### check for more detailed object dependencies:
    ### (use `checkObject` or similar)

    # do stuff for this event
    sim <- sim$MPBRedTopClimateInit(sim)

    # schedule future event(s)
    sim <- scheduleEvent(sim, start(sim) + params(sim)$MPBRedTopClimate$updateInterval, "MPBRedTopClimate", "update")
    sim <- scheduleEvent(sim, start(sim) + params(sim)$MPBRedTopClimate$.plotInitialTime, "MPBRedTopClimate", "plot")
    sim <- scheduleEvent(sim, start(sim) + params(sim)$MPBRedTopClimate$.saveInitialTime, "MPBRedTopClimate", "save")

  } else if (eventType=="plot") {
    # ! ----- EDIT BELOW ----- ! #
    # do stuff for this event
    Plot(sim[[globals(sim)$climateMap]], legendRange=c(0,50))

    # schedule future event(s)
    sim <- scheduleEvent(sim, time(sim) + params(sim)$MPBRedTopClimate$.plotInterval,
                         "MPBRedTopClimate", "plot")

    # ! ----- STOP EDITING ----- ! #
  } else if (eventType=="save") {
    # ! ----- EDIT BELOW ----- ! #
    # do stuff for this event

    # e.g., call your custom functions/methods here
    # you can define your own methods below this `doEvent` function

    # schedule future event(s)

    # e.g.,
    # sim <- scheduleEvent(sim, time(sim) + increment, "MPBRedTopClimate", "save")

    # ! ----- STOP EDITING ----- ! #
  } else if (eventType=="update") {
    # ! ----- EDIT BELOW ----- ! #
    # do stuff for this event
    sim <- sim$MPBRedTopClimateUpdate(sim)
    #print(sim[[globals(sim)$climateMap]])

    # schedule future event(s)
    sim <- scheduleEvent(sim, time(sim) + params(sim)$MPBRedTopClimate$updateInterval,
                         "MPBRedTopClimate", "update")

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
MPBRedTopClimateInit <- function(sim) {

  # # ! ----- EDIT BELOW ----- ! #

  # dummy climate map for now
  nx <- params(sim)$MPBRedTopClimate$nx
  ny <- params(sim)$MPBRedTopClimate$ny
  template <- raster(nrows=ny, ncols=nx, xmn=-nx/2, xmx=nx/2, ymn=-ny/2, ymx=ny/2)
  speedup <- max(1, nx/5e2)
  inMemory <- TRUE

  climate <- gaussMap(template, scale=300, var=0.03, speedup=speedup, inMemory=inMemory)
  climate <- climate/maxValue(climate)*10
  names(climate) <- "climate"
  setColors(climate, n=51) <- rev(brewer.pal(11, "RdYlBu"))

  sim[[globals(sim)$climateMap]] <- climate

  # ! ----- STOP EDITING ----- ! #

  return(invisible(sim))
}

### template for save events
MPBRedTopClimateSave <- function(sim) {
  # ! ----- EDIT BELOW ----- ! #
  # do stuff for this event
  sim <- saveFiles(sim)

  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}

### template for your event1
MPBRedTopClimateUpdate <- function(sim) {
  # ! ----- EDIT BELOW ----- ! #

  # dummy function that increases climate map values
  sim[[globals(sim)$climateMap]] <- setValues(sim[[globals(sim)$climateMap]],
                                              getValues(sim[[globals(sim)$climateMap]]) + 1 + rnorm(1, 2.9))

  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}

### add additional events as needed by copy/pasting from above
