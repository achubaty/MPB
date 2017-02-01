
# Everything in this file gets sourced during simInit, and all functions and objects
#  are put into the simList. To use objects and functions, use sim$xxx.
defineModule(sim, list(
  name = "mpbRedTopGrowth",
  description = "Mountain Pine Beetle Red Top Growth Model: Short-run Potential for Establishment, Eruption, and Spread",
  keywords = c("mountain pine beetle, outbreak dynamics, eruptive potential, spread, climate change, twitch response"),
  authors = c(person(c("Barry", "J"), "Cooke", email = "barry.cooke@ontario.ca", role = c("aut", "cre")),
              person(c("Alex", "M"), "Chubaty", email = "alexander.chubaty@canada.ca", role = c("aut", "cre"))),
  childModules = character(),
  version = numeric_version("0.0.1"),
  spatialExtent = raster::extent(rep(NA_real_, 4)),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year",
  citation = list(),
  reqdPkgs = list("ggplot2", "raster"),
  parameters = rbind(
    defineParameter(".plotInitialTime", "numeric", start(sim), NA, NA, "This describes the simulation time at which the first plot event should occur"),
    defineParameter(".plotInterval", "numeric", NA, NA, NA, "This describes the interval between plot events"),
    defineParameter(".saveInitialTime", "numeric", NA, NA, NA, "This describes the simulation time at which the first save event should occur"),
    defineParameter(".saveInterval", "numeric", NA, NA, NA, "This describes the interval between save events"),
    defineParameter(".useCache", "numeric", FALSE, NA, NA, "Should this entire module be run with caching activated?"),
    defineParameter("dataset", "character", "Berryman1979_forced", NA, NA, "Which dataset to use for stand dynamic model fitting. One of 'Berrman1979_fit', 'Berrman1979_forced' (default). Others to be implemented later."),
    defineParameter("growthInterval", "numeric", 1, NA, NA, "This describes the interval time between growth events")
  ),
  inputObjects = rbind(
    expectsInput("pineMap", "RasterLayer", desc = "Map of pine available for MPB."),
    expectsInput("massAttacksT", "RasterLayer", desc = ""),
    expectsInput("massAttacksTminus1", "RasterLayer", desc = "")
  ),
  outputObjects = rbind(
    createsOutput("growthData", "data.frame", desc = ""),
    createsOutput("massAttacksT", "RasterLayer", desc = ""),
    createsOutput("massAttacksTminus1", "RasterLayer", desc = "")
  )
))

## event types
#   - type `init` is required for initiliazation

doEvent.mpbRedTopGrowth <- function(sim, eventTime, eventType, debug = FALSE) {
  switch(eventType,
    "init" = {
      ### check for more detailed object dependencies:
      ### (use `checkObject` or similar)
  
      # do stuff for this event
      sim <- sim$mpbRedTopGrowthInit(sim)
      sim <- sim$mpbRedTopGrowthPlotInit(sim)
  
      # schedule future event(s)
      #sim <- scheduleEvent(sim, P(sim)$.plotInitialTime, "mpbRedTopGrowth", "plot")
    },
    "grow" = {
      # do stuff for this event
      sim <- sim$mpbRedTopGrowthGrow(sim)
      
      # schedule future event(s)
      sim <- scheduleEvent(sim, time(sim) + 1, "mpbRedTopGrowth", "grow")
    },
    "plot" = {
      # ! ----- EDIT BELOW ----- ! #
      # do stuff for this event
      sim <- sim$mpbRedTopGrowthPlot(sim)
      
      # schedule future event(s)
      sim <- scheduleEvent(sim, time(sim) + 1, "mpbRedTopGrowth", "plot")
  
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

mpbRedTopGrowthInit <- function(sim) {
  # # ! ----- EDIT BELOW ----- ! #
  stopifnot(res(sim$massAttacksT) == res(sim$massAttacksTminus1),
            extent(sim$massAttacksT) == extent(sim$massAttacksTminus1),
            xres(sim$massAttacksT) == yres(sim$massAttacksT))
  
  sim$growthData <- switch(P(sim)$dataset,
    "Berryman1979_fit" = {
      ## Berryman1979_forced
      data.frame(
        year = c(1:15),
        log10Rt = c(-3.1, -2.75, -2.7, -2.4, -2.3, -1.2, -1, 0.2, 0.9, 0.65,
                    1.05, 0.95, 1.1, 1.5, 1.85),
        log10Xtm1 = c(0.35, 0.4, 0.1, -0.4, -0.65, 0.3, 1, 0.75, 1.2, -0.7,
                      -0.4, 0.2, 0.45, 0.3, -0.78),
        study = c(rep("Tunnock 1970", 9), rep("Parker 1973", 6)),
        stringsAsFactors = TRUE
      )
    },
    "Berryman1979_forced" = {
      ## same as Berryman1979_fit
      data.frame(
        year = c(1:15),
        log10Rt = c(-3.1, -2.75, -2.7, -2.4, -2.3, -1.2, -1, 0.2, 0.9, 0.65,
                    1.05, 0.95, 1.1, 1.5, 1.85),
        log10Xtm1 = c(0.35, 0.4, 0.1, -0.4, -0.65, 0.3, 1, 0.75, 1.2, -0.7,
                      -0.4, 0.2, 0.45, 0.3, -0.78),
        study = c(rep("Tunnock 1970", 9), rep("Parker 1973", 6)),
        stringsAsFactors = TRUE
      )
    }
  )
  
  ## define growth function (from regression) for each dataset
  growthFunction <- switch(P(sim)$dataset,
     "Berryman1979_fit" = {
       function(x) {
         m <- lm(log10Rt ~ poly(log10Xtm1, 3, raw = TRUE), data = sim$growthData)
         unname(predict(m, newdata = data.frame(log10Xtm1 = x)))
       }
     },
     "Berryman1979_force" = {
       function(x) {
         poly3.params <- c(1.1, -0.2, -0.9, -0.24)
         (poly3.params[4] * x^3 + poly3.params[3] * x^2 + poly3.params[2] * x + poly3.params[1])
       }
     }
  )
  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}

mpbRedTopGrowthPlotInit <- function(sim) {
  # ! ----- EDIT BELOW ----- ! #
  # do stuff for this event

  ### see ggplot docs at http://docs.ggplot2.org/current/
  gg <- ggplot(sim$growthData) +
    geom_point(aes(x = log10Xtm1, y = log10Rt, shape = study)) +
    scale_shape(solid = FALSE) +
    xlim(-1.0, 1.5) + ylim(-3.2, 2.0) +
    labs(title = switch(P(sim)$dataset,
                        "Berryman1979_fit" = "Berryman (1979) [fit]",
                        "Berryman1979_forced" = "Berryman (1979) [forced]"
                ),
         x = "X[t-1] (log10 trees/ha/yr)",
         y = "R[t] = log10 x[t]/x[t-1]") +
    geom_hline(aes(yintercept = 0)) +
    switch(P(sim)$dataset,
      "Berryman1979_fit" = {
        stat_smooth(aes(x = log10Xtm1, y = log10Rt), method = "lm",
                    formula = y ~ poly(x, 3, raw = TRUE))
      },
      "Berryman1979_forced" = {
        stat_function(fun = sim$growthFunction)
      }
    )
  
  ### Plot it!
  Plot(gg)

  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}

mpbRedTopGrowthPlot <- function(sim) {
  # ! ----- EDIT BELOW ----- ! #
  # do stuff for this event
  Plot(sim$massAttacksT)
  
  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}

mpbRedTopGrowthGrow <- function(sim) {
  ## determine the actual growth based on the actual number of attacked trees/ha
  xt <- function(xtminus1) {
    map.res <- xres(xtminus1)
    per.ha <- 10^sim$growthFunction(log10(xtminus1)) * xtminus1
    return(map.res * per.ha)
  }
  
  sim$massAttacksT <- Xt(sim$massAttacksTminus1)  ## TO DO: ensure tmp rasters don't proliferate!
}
