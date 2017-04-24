
# Everything in this file gets sourced during simInit, and all functions and objects
#  are put into the simList. To use objects and functions, use sim$xxx.
defineModule(sim, list(
  name = "mpbRedTopSpread",
  description = "Mountain Pine Beetle Red Top Growth Model: Short-run Potential for Establishment, Eruption, and Spread",
  keywords = c("mountain pine beetle, outbreak dynamics, eruptive potential, spread, climate change, twitch response"),
  authors = c(
    person(c("Alex", "M"), "Chubaty", email = "alexander.chubaty@canada.ca", role = c("aut", "cre")),
    person(c("Eliot", "J B"), "McIntire", email = "eliot.mcintire@canada.ca", role = c("aut")),
    person(c("Barry", "J"), "Cooke", email = "barry.cooke@ontario.ca", role = c("aut"))
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
    defineParameter(".useCache", "numeric", FALSE, NA, NA, "Should this entire module be run with caching activated?"),
    defineParameter("asymmetry", "numeric", 2, NA, NA, "The magnitude of the directional bias of spread"),
    defineParameter("asymmetryAngle", "numeric", 90, NA, NA, "The direction of the spread bias, in degrees from north"),
    defineParameter("dispersalInterval", "numeric", 1, NA, NA, "This describes the interval time between dispersal events"),
    defineParameter("dispersalKernel", "character", "NegExp", NA, NA, "Name of the dispersal kernel to use"),
    defineParameter("dispersalKernelLambda", "numeric", 1, NA, NA, "Dispersal kernel lambda parameter")
  ),
  inputObjects = bind_rows(
    expectsInput("massAttacksDT", "data.table", "Current MPB attack map (number of red attacked trees)."),
    expectsInput("massAttacksMap", "RasterStack", "Current MPB attack map (number of red attacked trees).")
  ),
  outputObjects = bind_rows(
    createsOutput("massAttacksDT", "data.table", "Current MPB attack map (number of red attacked trees).")
  )
))

## event types
#   - type `init` is required for initilization

doEvent.mpbRedTopSpread <- function(sim, eventTime, eventType, debug = FALSE) {
  switch(eventType,
    "init" = {
      # do stuff for this event
      sim <- sim$mpbRedTopSpreadInit(sim)
  
      # schedule future event(s)
      sim <- scheduleEvent(sim, time(sim) + P(sim)$mpbRedTopSpread$dispersalInterval,
                           "mpbRedTopSpread", "dispersal")
      sim <- scheduleEvent(sim, P(sim)$mpbRedTopSpread$.plotInitialTime, "mpbRedTopSpread", "plot")
      sim <- scheduleEvent(sim, P(sim)$mpbRedTopSpread$.saveInitialTime, "mpbRedTopSpread", "save")
    },
    "dispersal" = {
      # ! ----- EDIT BELOW ----- ! #
      # do stuff for this event
  
      sim <- sim$mpbRedTopSpreadDispersal(sim)
  
      sim <- scheduleEvent(sim, time(sim) + P(sim)$mpbRedTopSpread$dispersalInterval,
                           "mpbRedTopSpread", "dispersal")
  
      # ! ----- STOP EDITING ----- ! #
    },
    "plot" = {
      # ! ----- EDIT BELOW ----- ! #

      plot(amc::dt2raster(sim$mpbSpreadDT))
      
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
    load(file.path(modulePath(sim), "mpbRedTopSpread", "data", "west.boreal.RData"), envir = envir(sim))
  }
  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}

## event functions
#   - follow the naming convention `modulenameEventtype()`;
#   - `modulenameInit()` function is required for initiliazation;
#   - keep event functions short and clean, modularize by calling subroutines from section below.

### initilization
mpbRedTopSpreadInit <- function(sim) {
  ## dispersal kernel
  sim$dispKern <- switch(
    P(sim)$dispersalKernel,
    "NegExp" = function(disFar, disNear, lambda) {
      (1 - exp(-lambda * disFar)) - (1 - exp(-lambda * disNear))
    }
  )

  return(invisible(sim))
}

### plotting
mpbRedTopSpreadPlot <- function(sim) {
  # ! ----- EDIT BELOW ----- ! #
  # do stuff for this event
  #Plot("object")

  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}

### spread
mpbRedTopSpreadDispersal <- function(sim) {
  ## use 1125 trees/ha, per Whitehead & Russo (2005), Cooke & Carroll (unpublished)
  MAXTREES <- round(1125 * (res(sim$massAttacksMap) / 100) ^ 2)
  
  ## asymmetric spread (biased eastward)
  out <- spread2(a, start = loci, spreadProb = 1, asRaster = FALSE, iterations = 0,
                 circle = TRUE,
                 asymmetry = P(sim)$mpbRedTopSpread$asymmetry,
                 asymmetryAngle = P(sim)$mpbRedTopSpread$asymmetryAngle,
                 returnDistances = TRUE, returnFrom = TRUE)
  set(out, , "abundanceActive", MAXTREES)
  set(out, , "abundanceSettled", 0)
  
  done <- FALSE
  while (!done) {
    out <- spread2(a, start = out, spreadProb = 1, asRaster = FALSE, iterations = 1,
                   circle = TRUE, returnDistances = TRUE, returnFrom = TRUE)
    set(out, , "order", seq_len(NROW(out)))
    attribs <- attr(out, "spreadState")
    
    outInactive <- out[!attr(out, "spreadState")$whActive, ]
    setkey(outInactive, initialPixels, pixels)
    setkey(out, initialPixels, from)
    outW_i_columns <- out[outInactive]
    outWLag1B <- outW_i_columns[which(state != "inactive")]
    
    outWLag1B[, abundanceSettled := pmin(
      MAXTREES,
      round(pine[pixels] * sim$dispKern(distance, i.distance, P(sim)$dispersalKernelLambda) *
              i.abundanceActive / (.N))
    ), by = "from"]
    outWLag1B[, abundanceActive := pmin(
      MAXTREES,
      round((1 - pine[pixels] * sim$dispKern(distance, i.distance, P(sim)$dispersalKernelLambda)) *
              i.abundanceActive / (.N))
    ), by = "from"]
    set(outWLag1B, , grep(colnames(outWLag1B), pattern = "^i\\.", value = TRUE), NULL)
    
    out <- rbindlist(list(outInactive, outWLag1B), fill = TRUE)
    setattr(out, "spreadState", attribs)
    if (sum(out[which(out$state != "inactive")]$abundanceSettled) == 0) {
      done <- TRUE
    }
  }

  sim$mpbSpreadDT <- out
  rm(out)
  
  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}

## convert data.table objects t raster for plotting etc.
## eventually this will move to `amc` package
dt2raster <- function(dt, r, val) {
  stopifnot(is(dt, "data.table"),
            all(c("ID", "X", "Y") %in% colnames(dt)),
            is(r, "Raster"),
            is.character(val))
  
  xy <- SpatialPoints(cbind(dt$X, dt$Y))
  ids <- cellFromXY(r, xy)
  tmp <- data.table(ID  = ids, VALUE = dt[[val]])
  tmp <- tmp[, VALUE := sum(VALUE), by = ID]
  setkey(tmp, ID)
  
  rout <- r
  if (length(tmp$ID)) rout[tmp$ID] <- tmp$VALUE
  if (ncell(rout) - length(tmp$ID) > 0) rout[!tmp$ID] <- NA
  return(rout)
}
