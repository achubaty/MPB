
# Everything in this file gets sourced during simInit, and all functions and objects
#  are put into the simList. To use objects and functions, use sim$xxx.
defineModule(sim, list(
  name="MPBRedTopLandscape",
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
    defineParameter("nx", "numeric", 100L, NA, NA, "size of map (number of pixels) in the x dimension"),
    defineParameter("ny", "numeric", 100L, NA, NA, "size of map (number of pixels) in the y dimension"),
    defineParameter("stackName", "character", "forestLandscape", NA, NA, "name of the RasterStack")
  ),
  inputObjects=data.frame(objectName=character(),
                          objectClass=character(),
                          other=character(), stringsAsFactors=FALSE),
  outputObjects=data.frame(objectName=globals(sim)$stackName,
                           objectClass="RasterStack",
                           other=NA_character_, stringsAsFactors=FALSE)
))

## event types
#   - type `init` is required for initiliazation

doEvent.MPBRedTopLandscape = function(sim, eventTime, eventType, debug=FALSE) {
  if (eventType=="init") {
    ### check for more detailed object dependencies:
    ### (use `checkObject` or similar)

    # do stuff for this event
    sim <- sim$MPBRedTopLandscapeInit(sim)

    # schedule future event(s)
    sim <- scheduleEvent(sim, start(sim) + params(sim)$MPBRedTopLandscape$.plotInitialTime, "MPBRedTopLandscape", "plot")
    sim <- scheduleEvent(sim, start(sim) + params(sim)$MPBRedTopLandscape$.saveInitialTime, "MPBRedTopLandscape", "save")
  } else if (eventType=="plot") {
    # ! ----- EDIT BELOW ----- ! #
    # do stuff for this event
    Plot(sim[[globals(sim)$stackName]])

    # schedule future event(s)
    sim <- scheduleEvent(sim, time(sim) + params(sim)$MPBRedTopLandscape$.plotInterval,
                         "MPBRedTopLandscape", "plot")

    # ! ----- STOP EDITING ----- ! #
  } else if (eventType=="save") {
    # ! ----- EDIT BELOW ----- ! #
    # do stuff for this event

    # e.g., call your custom functions/methods here
    # you can define your own methods below this `doEvent` function

    # schedule future event(s)

    # e.g.,
    # sim <- scheduleEvent(sim, time(sim) + increment, "MPBRedTopLandscape", "save")

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
MPBRedTopLandscapeInit <- function(sim) {

  # # ! ----- EDIT BELOW ----- ! #

  ### temporarily use dummy map that are randomly generated
  ###  this code was copied from the randomLandscapes sample module
  nx <- params(sim)$MPBRedTopLandscape$nx
  ny <- params(sim)$MPBRedTopLandscape$ny
  template <- raster(nrows=ny, ncols=nx, xmn=-nx/2, xmx=nx/2, ymn=-ny/2, ymx=ny/2)
  speedup <- max(1, nx/5e2)
  inMemory <- TRUE

  DEM <- gaussMap(template, scale=300, var=0.03, speedup=speedup, inMemory=inMemory)
  DEM[] <- round(getValues(DEM),1)*1000
  forestAge <- gaussMap(template, scale=10, var=0.1, speedup=speedup, inMemory=inMemory)
  forestAge[] <- round(getValues(forestAge),1)*20
  percentPine <- gaussMap(template, scale=50, var=1, speedup=speedup, inMemory=inMemory)
  percentPine[] <- round(getValues(percentPine), 1)

  # Scale them as needed
  forestAge <- forestAge/maxValue(forestAge)*100
  percentPine <- percentPine/maxValue(percentPine)*100

  # Make layers that are derived from other layers
  habitatQuality <- (DEM+10 + (forestAge+2.5)*10)/100
  habitatQuality <- habitatQuality/maxValue(habitatQuality)

  # Stack them into a single stack and assign to global env
  mapStack <- stack(DEM, forestAge, habitatQuality, percentPine)
  names(mapStack) <- c("DEM", "forestAge", "habitatQuality", "percentPine")

  setColors(mapStack) <- list(DEM=grDevices::terrain.colors(100),
                              forestAge=brewer.pal(9,"BuGn"),
                              habitatQuality=brewer.pal(8,"Spectral"),
                              percentPine=brewer.pal(9,"Greens"))
  sim[[globals(sim)$stackName]] <- mapStack

  # ! ----- STOP EDITING ----- ! #

  return(invisible(sim))
}

### template for save events
MPBRedTopLandscapeSave <- function(sim) {
  # ! ----- EDIT BELOW ----- ! #
  # do stuff for this event
  sim <- saveFiles(sim)

  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}
