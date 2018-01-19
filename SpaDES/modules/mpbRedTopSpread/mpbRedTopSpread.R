
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
  reqdPkgs = list("amc", "data.table", "quickPlot", "raster", "RColorBrewer", "reproducible"),
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
      sim <- scheduleEvent(sim, time(sim) + P(sim)$dispersalInterval,
                           "mpbRedTopSpread", "dispersal")
      sim <- scheduleEvent(sim, P(sim)$.plotInitialTime, "mpbRedTopSpread", "plot")
      sim <- scheduleEvent(sim, P(sim)$.saveInitialTime, "mpbRedTopSpread", "save")
    },
    "dispersal" = {
      # ! ----- EDIT BELOW ----- ! #
      # do stuff for this event

      sim <- sim$mpbRedTopSpreadDispersal(sim)

      sim <- scheduleEvent(sim, time(sim) + P(sim)$dispersalInterval,
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
    f <- file.path(modulePath(sim), "mpbRedTopSpread", "data", "studyArea.kml")
    prj <- paste("+proj=aea +lat_1=47.5 +lat_2=54.5 +lat_0=0 +lon_0=-113",
                 "+x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0")
    sim$studyArea <- readOGR(f, "studyArea.kml") %>%
      sp::spTransform(., prj)
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
  
  r <- sim$massAttacksMap[[paste0("X", start(sim))]]
  loci <- sim$massAttacksDT$ID
  
  ## asymmetric spread (biased eastward)
  out <- spread2(r, start = loci, spreadProb = 1, asRaster = FALSE, iterations = 0,
                 circle = TRUE,
                 asymmetry = P(sim)$asymmetry,
                 asymmetryAngle = P(sim)$asymmetryAngle,
                 returnDistances = TRUE, returnFrom = TRUE)
  set(out, , "abundanceActive", MAXTREES)
  set(out, , "abundanceSettled", 0)
  
  done <- FALSE
  while (!done) {
    out <- spread2(r, start = out, spreadProb = 1, asRaster = FALSE, iterations = 1,
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
      round(sim$massAttacksDT$PROPPINE[pixels] * sim$dispKern(distance, i.distance, P(sim)$dispersalKernelLambda) *
              i.abundanceActive / (.N))
    ), by = "from"]
    outWLag1B[, abundanceActive := pmin(
      MAXTREES,
      round((1 - sim$massAttacksDT$PROPPINE[pixels] * sim$dispKern(distance, i.distance, P(sim)$dispersalKernelLambda)) *
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
