
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
  reqdPkgs = list("amc", "data.table", "quickPlot", "raster", "RColorBrewer", "reproducible"),
  parameters = rbind(
    defineParameter(".plotInitialTime", "numeric", NA, NA, NA, "This describes the simulation time at which the first plot event should occur"),
    defineParameter(".plotInterval", "numeric", 1, NA, NA, "This describes the interval between plot events"),
    defineParameter(".saveInitialTime", "numeric", NA, NA, NA, "This describes the simulation time at which the first save event should occur"),
    defineParameter(".saveInterval", "numeric", NA, NA, NA, "This describes the interval between save events"),
    defineParameter(".useCache", "numeric", FALSE, NA, NA, "Should this entire module be run with caching activated?")
  ),
  inputObjects = bind_rows(
    expectsInput("studyArea", "SpatialPolygons", "The study area to which all maps will be cropped and reprojected.", sourceURL = NA)
  ),
  outputObjects = bind_rows(
    createsOutput("massAttacksMap", "RasterStack", "Historical MPB attack maps (number of red attacked trees).")
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
      sim <- scheduleEvent(sim, P(sim)$.plotInitialTime,
                           "mpbMassAttacksData", "plot", .last() - 1)
      sim <- scheduleEvent(sim, P(sim)$.saveInitialTime,
                           "mpbMassAttacksData", "save", .last())
    },
    "plot" = {
      # ! ----- EDIT BELOW ----- ! #
      # do stuff for this event
      Plot(sim$massAttacksMap)
  
      # schedule future event(s)
      sim <- scheduleEvent(sim, time(sim) + P(sim)$.plotInterval,
                           "mpbMassAttacksData", "plot", .last() - 1)
  
      # ! ----- STOP EDITING ----- ! #
    },
    warning(paste("Undefined event type: '", events(sim)[1, "eventType", with = FALSE],
                  "' in module '", events(sim)[1, "moduleName", with = FALSE], "'", sep = ""))
  )
  return(invisible(sim))
}

.inputObjects <- function(sim) {
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
  
  # TODO: incorporate code from MPB_maps.R to create the raster layers
  f <- file.path(modulePath(sim), "mpbMassAttacksData", "data", "mpb_bcab_boreal_1998-2016.tif")
  stopifnot(file.exists(f))

  ## all MPB data (all years -- missing 1999 and 2000)
  sim$massAttacksMap <- Cache(amc::cropReproj, f, sim$studyArea, c("X1998", paste0("X", 2001:2016)))
  
  # TODO: use fasterize (requires use of sf)
  sim$rstStudyArea <- Cache(rasterize, sim$studyArea, sim$massAttacksMap)
  setColors(sim$massAttacksMap) <- rep(list(brewer.pal(9, "YlOrRd")), nlayers(sim$massAttacksMap))
  
  ## data.table of MPB attacks in study area
  sim$massAttacksDT <- as.data.table(rasterToPoints(sim$massAttacksMap, fun = function(x) { x > 0 }))
  colnames(sim$massAttacksDT) <- c("x", "y", "NUMTREES") ## NUMTREES is number of attacked trees!
  
  # join with pine data.table
  sim$massAttacksDT <- sim$massAttacksDT[sim$pineMapDT]
  
  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}
