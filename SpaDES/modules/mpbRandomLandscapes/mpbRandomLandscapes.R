
# Everything in this file gets sourced during simInit, and all functions and objects
#  are put into the simList. To use objects and functions, use sim$xxx.
defineModule(sim, list(
  name = "mpbRandomLandscapes",
  description = "Mountain Pine Beetle Red Top Growth Model: Short-run Potential for Establishment, Eruption, and Spread",
  keywords = c("mountain pine beetle, outbreak dynamics, eruptive potential, spread, climate change, twitch response"),
  authors = c(person(c("Barry", "J"), "Cooke", email = "Barry.Cooke@NRCan.gc.ca", role = c("aut", "cre")),
            person(c("Alex", "M"), "Chubaty", email = "Alexander.Chubaty@NRCan.gc.ca", role = c("aut", "cre"))),
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
    defineParameter("nx", "numeric", 100L, NA, NA, "size of map (number of pixels) in the x dimension"),
    defineParameter("ny", "numeric", 100L, NA, NA, "size of map (number of pixels) in the y dimension")
  ),
  inputObjects = rbind(
    expectsInput(NA_character_, NA_character_, NA_character_, NA_character_)
  ),
  outputObjects = rbind(
    createsOutput("DEM", "RasterLayer"),
    createsOutput("habitatQuality", "RasterLayer"),
    createsOutput("pineMap", "RasterLayer", "map of percent pine")
  )
))

## event types
#   - type `init` is required for initiliazation

doEvent.mpbRandomLandscapes <- function(sim, eventTime, eventType, debug = FALSE) {
  switch(eventType,
    "init" = {
      ### check for more detailed object dependencies:
      ### (use `checkObject` or similar)
  
      # do stuff for this event
      sim <- sim$mpbRandomLandscapesInit(sim)
  
      # schedule future event(s)
      sim <- scheduleEvent(sim, start(sim) + P(sim)$.plotInitialTime, "mpbRandomLandscapes", "plot")
      sim <- scheduleEvent(sim, start(sim) + P(sim)$.saveInitialTime, "mpbRandomLandscapes", "save")
    },
    "plot" = {
      # ! ----- EDIT BELOW ----- ! #
      # do stuff for this event
      Plot(sim[[globals(sim)$stackName]])
  
      # schedule future event(s)
      sim <- scheduleEvent(sim, time(sim) + P(sim)$.plotInterval, "mpbRandomLandscapes", "plot")
  
      # ! ----- STOP EDITING ----- ! #
    },
    "save" = {
      # ! ----- EDIT BELOW ----- ! #
      # do stuff for this event
  
      # e.g., call your custom functions/methods here
      # you can define your own methods below this `doEvent` function
  
      # schedule future event(s)
  
      # e.g.,
      # sim <- scheduleEvent(sim, time(sim) + increment, "mpbRandomLandscapes", "save")
  
      # ! ----- STOP EDITING ----- ! #
      },
      warning(paste("Undefined event type: '", events(sim)[1, "eventType", with = FALSE],
                    "' in module '", events(sim)[1, "moduleName", with = FALSE], "'", sep = ""))
  )
  return(invisible(sim))
}

## event functions
#   - follow the naming convention `modulenameEventtype()`;
#   - `modulenameInit()` function is required for initiliazation;
#   - keep event functions short and clean, modularize by calling subroutines from section below.

### template initilization
mpbRandomLandscapesInit <- function(sim) {
  # # ! ----- EDIT BELOW ----- ! #
  nx <- P(sim)$nx
  ny <- P(sim)$ny
  template <- raster(nrows = ny, ncols = nx, xmn = -nx/2, xmx = nx/2, ymn = -ny/2, ymx = ny/2)
  speedup <- max(1, nx / 5e2)
  inMemory <- TRUE

  DEM <- gaussMap(template, scale = 300, var = 0.03, speedup = speedup, inMemory = inMemory)
  DEM[] <- round(getValues(DEM), 1) * 1000
  setColors(DEM) <- grDevices::terrain.colors(100)
  sim$DEM <- DEM
  
  pineMap <- gaussMap(template, scale = 50, var = 1, speedup = speedup, inMemory = inMemory)
  pineMap[] <- round(getValues(pineMap), 1)
  pineMap <- pineMap / maxValue(pineMap) * 100 ## scale value to give percentage pine
  setColors(pnieMap) <- brewer.pal(9, "Greens")
  sim$pineMap <- pineMap

  # Make layers that are derived from other layers
  habitatQuality <- (DEM + 10) * pineMap / 100
  habitatQuality <- habitatQuality / maxValue(habitatQuality)
  setColors(habitatQuality) <- brewer.pal(8, "Spectral")
  sim$habitatQuality <- habitatQuality

  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}

### template for save events
mpbRandomLandscapesSave <- function(sim) {
  # ! ----- EDIT BELOW ----- ! #
  # do stuff for this event
  sim <- saveFiles(sim)

  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}
