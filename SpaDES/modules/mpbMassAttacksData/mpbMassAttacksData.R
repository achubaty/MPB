
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
  reqdPkgs = list("amc", "data.table", "raster", "RColorBrewer"),
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
  f <- file.path(modulePath(sim), "mpbMassAttacksData", "data", "mpb_bcab_boreal_1997-2011.tif")
  stopifnot(file.exists(f))

  ## all MPB data (all years)
  sim$massAttacksMap <- Cache(amc::cropReproj, f, sim$studyArea, paste0("X", 1997:2011))
  setColors(sim$massAttacksMap) <- rep(list(brewer.pal(9, "YlOrRd")), nlayers(sim$massAttacksMap))

  ## only the start year's non-zero and non-NA data
  ids <- which(!is.na(sim$massAttacksMap[[paste0("X", start(sim))]])) ## need non-zeros only
  sim$massAttacksDT <- data.table(
    ID = ids,
    X = xyFromCell(sim$massAttacksMap[[paste0("X", start(sim))]], cell = ids)$x,
    Y = xyFromCell(sim$massAttacksMap[[paste0("X", start(sim))]], cell = ids)$y,
    NUMTREES = sim$massAttacksMap[[paste0("X", start(sim))]][ids]
  )
  
  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}
