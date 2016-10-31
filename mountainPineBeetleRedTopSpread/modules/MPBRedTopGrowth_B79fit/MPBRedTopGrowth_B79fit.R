
# Everything in this file gets sourced during simInit, and all functions and objects
#  are put into the simList. To use objects and functions, use sim$xxx.
defineModule(sim, list(
  name="MPBRedTopGrowth_B79fit",
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
  reqdPkgs=list("ggplot2"),
  parameters=rbind(
    defineParameter(".plotInitialTime", "numeric", NA, NA, NA, "This describes the simulation time at which the first plot event should occur"),
    defineParameter(".plotInterval", "numeric", 1, NA, NA, "This describes the interval between plot events"),
    defineParameter(".saveInitialTime", "numeric", NA, NA, NA, "This describes the simulation time at which the first save event should occur"),
    defineParameter(".saveInterval", "numeric", NA, NA, NA, "This describes the interval between save events"),
    defineParameter("growthInterval", "numeric", 1, NA, NA, "This describes the interval time between growth events")
  ),
  inputObjects=data.frame(objectName=c(globals(sim)$stackName, "massAttacksT", "massAttacksTminus1"),
                          objectClass=c("RasterStack", "RasterLayer", "RasterLayer"),
                          other=c(NA_character_, NA_character_, NA_character_),
                          stringsAsFactors=FALSE),
  outputObjects=data.frame(objectName=c("Berryman1979data", "massAttacksT", "massAttacksTminus1"),
                           objectClass=c("data.frame", "RasterLayer", "RasterLayer"),
                           other=c(NA_character_, NA_character_, NA_character_),
                           stringsAsFactors=FALSE)
))

## event types
#   - type `init` is required for initiliazation

doEvent.MPBRedTopGrowth_B79fit = function(sim, eventTime, eventType, debug=FALSE) {
  if (eventType=="init") {
    ### check for more detailed object dependencies:
    ### (use `checkObject` or similar)

    # do stuff for this event
    sim <- sim$MPBRedTopGrowth_B79fitInit(sim)

    # schedule future event(s)
    sim <- scheduleEvent(sim, start(sim)+params(sim)$MPBRedTopGrowth_B79fit$.plotInitialTime, "MPBRedTopGrowth_B79fit", "plot")
    sim <- scheduleEvent(sim, start(sim)+params(sim)$MPBRedTopGrowth_B79fit$.saveInitialTime, "MPBRedTopGrowth_B79fit", "save")
  } else if (eventType=="plot") {
    # ! ----- EDIT BELOW ----- ! #
    # do stuff for this event

    sim <- sim$MPBRedTopGrowth_B79fitPlot(sim)

    # e.g., call your custom functions/methods here
    # you can define your own methods below this `doEvent` function

    # schedule future event(s)

    # e.g.,
    # sim <- scheduleEvent(sim, time(sim) + increment, "MPBRedTopGrowth_B79fit", "plot")

    # ! ----- STOP EDITING ----- ! #
  } else if (eventType=="save") {
    # ! ----- EDIT BELOW ----- ! #
    # do stuff for this event

    # e.g., call your custom functions/methods here
    # you can define your own methods below this `doEvent` function

    # schedule future event(s)

    # e.g.,
    # sim <- scheduleEvent(sim, time(sim) + increment, "MPBRedTopGrowth_B79fit", "save")

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
MPBRedTopGrowth_B79fitInit <- function(sim) {

  # # ! ----- EDIT BELOW ----- ! #

  sim$Berryman1979data <- data.frame(
    yearRedTopsObserved = c(1:15),
    laggedlog10redTopsObserved = c(-3.1,-2.75,-2.7,-2.4,-2.3,-1.2,-1,0.2,0.9,0.65,1.05,.95,1.1,1.5,1.85),
    log10changeInRedTopsObserved = c(0.35,0.4,0.1,-0.4,-0.65,0.3,1,0.75,1.2,-0.7,-0.4,0.2,0.45,0.3,-0.78),
    study = c(rep("Tunnock 1970", 9), rep("Parker 1973", 6)),
    stringsAsFactors = TRUE
  )

  # ! ----- STOP EDITING ----- ! #

  return(invisible(sim))
}

### template for save events
MPBRedTopGrowth_B79fitSave <- function(sim) {
  # ! ----- EDIT BELOW ----- ! #
  # do stuff for this event
  sim <- saveFiles(sim)

  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}

### template for plot events
MPBRedTopGrowth_B79fitPlot <- function(sim) {
  # ! ----- EDIT BELOW ----- ! #
  # do stuff for this event

  ### see ggplot docs at http://docs.ggplot2.org/current/

  ### this first part is identical to the corresponding step in B79forced
  gg <- ggplot(sim$Berryman1979data) +
    geom_point(aes(x = laggedlog10redTopsObserved,
                   y = log10changeInRedTopsObserved,
                   shape = study)) +
    scale_shape(solid = FALSE) +
    xlim(-3.2, 2) + ylim(-1.5, 1.5) +
    labs(title="Berryman (1979) [fit]", x="X[t-1] (log10 trees/ha/yr)", y="R[t] = log10 x[t]/x[t-1]") +
    geom_hline(aes(yintercept=0))

  ### below is unique to this module
  gg <- gg +
    stat_smooth(aes(x = laggedlog10redTopsObserved,
                    y = log10changeInRedTopsObserved),
                method = "lm",
                formula = y ~ poly(x, 3, raw=TRUE))

  ### Plot it!
  Plot(gg)

  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}

### template for your event1
MPBRedTopGrowth_B79fitEvent1 <- function(sim) {
  # ! ----- EDIT BELOW ----- ! #



  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}
