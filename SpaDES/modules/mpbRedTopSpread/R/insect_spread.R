#install_github("PredictiveEcology/SpaDES.tools@prepInputs")
#devtools::load_all("~/GitHub/SpaDES.tools")
library(data.table)
library(quickPlot)
library(raster)
library(SpaDES.tools)

._NUMCPUS_. <- parallel::detectCores() / 2

dev.useRSGD(FALSE)
dev()

# inputs
XMAX <- YMAX <- 400
a <- raster(extent(0, XMAX, 0, YMAX), res = 1)
pine <- gaussMap(a) ## gaussMap resets DT threads, so be sure to reset it below
pine[] <- pine[] / maxValue(pine)
#pine[] <- 0.6 ## TEMPORARY
pix <- pine[]

setDTthreads(._NUMCPUS_.)

# parameter in the dispKern function, describes steepness of curve,
#  higher towards 1 is steeper, lower towards 0 is flatter
LAMBDA <- 0.12

dispKern <- function(disFar, disNear, lambda) {
  (1 - exp(-lambda * disFar)) - (1 - exp(-lambda * disNear))
}

loci <- sample(ncell(a), 100)

## max num red trees [= 1125 trees/ha * (MAPRES / 100)^2]
TOTAL <- round(1125 * (250 / 100)^2)
saturationDensity <- 332
asymm <- 5

out <- spread2(a, start = loci, spreadProb = 1, asRaster = FALSE, iterations = 0,
               asymmetry = asymm, asymmetryAngle = 0,
               circle = TRUE,
               returnDistances = TRUE, returnFrom = TRUE)
set(out, , "abundanceActive", 0)
set(out, , "abundanceSettled", 0)
set(out, , "abundanceReceived", 0)
set(out, , "Total", TOTAL)

system.time({
  for (year in 2012:2012) {
    ## within a season, disperse the beetles
    done <- FALSE
    while (!done) {
      nrowOut <- NROW(out)
      out <- spread2(a, start = out, spreadProb = 1, asRaster = FALSE, iterations = 1,
                     asymmetry = asymm, asymmetryAngle = 90,
                     circle = TRUE, allowOverlap = TRUE,
                     returnDistances = TRUE, returnFrom = TRUE)
      if (nrowOut != NROW(out)) {
        # might spread and have none that are <1 unit. Just go back to while loop
        set(out, , "order", seq_len(NROW(out)))
        attribs <- attr(out, "spreadState")

        setkey(out, initialPixels, from)
        out[, beetleSource := !is.na(abundanceActive),  by = c("initialPixels", "from")]
        receivers <- out[beetleSource == FALSE]
        sources <- out[beetleSource == TRUE]

        setkey(sources, initialPixels, pixels)
        setkey(receivers, initialPixels, from)
        outW_i_columns <- receivers[sources]
        outWLag1B <- outW_i_columns[beetleSource == FALSE]

        # Calculate the abundance received, as a function of distance
        outWLag1B[, abundanceReceived :=
                    ceiling(dispKern(effectiveDistance, i.effectiveDistance, LAMBDA) *
                                            proportion *
                                            i.Total + i.abundanceActive *
                                            proportion / sum(proportion)),
                  by = c("initialPixels", "from")] # Extra ones if they didn't settle
        # Calculate the abundance received, as a function of angle,
        # which was already calculated in spread2, and is called "proportion".
        # The pmin is about saturation density.
        outWLag1B[, abundanceSettled := pmin(floor(abundanceReceived * pix[pixels] *
                                                     saturationDensity /
                                                     sum(abundanceReceived * pix[pixels])),
                                             ceiling(abundanceReceived * pix[pixels])),
                  by = c("pixels")]
        outWLag1B[, abundanceActive := floor(abundanceReceived - abundanceSettled)]

        set(outWLag1B, , grep(colnames(outWLag1B), pattern = "^i\\.", value = TRUE), NULL)

        out <- rbindlist(list(sources, outWLag1B), fill = TRUE)

        # Squash down duplicates
        outSum <- out[, list(abundanceActive = sum(abundanceActive),
                             abundanceSettled = sum(abundanceSettled),
                             abundanceReceived = sum(abundanceReceived)),
                      by = c("initialPixels", "pixels")]
        out <- unique(out, by = c("initialPixels", "pixels"))
        set(out, , "abundanceActive", outSum$abundanceActive)
        set(out, , "abundanceSettled", outSum$abundanceSettled)
        set(out, , "abundanceReceived", outSum$abundanceReceived)
        set(out, , "Total", TOTAL)
        setattr(out, "spreadState", attribs)

        # Because of multiple starting loci, a given pixel can get overfull.
        overfull <- out[, sum(abundanceSettled), by = "pixels"]
        if (isTRUE(any(overfull$V1 > saturationDensity))) {
          pixelsWithTooMany <- overfull[V1 > saturationDensity]$pixels
          set(out, , "cumsumAbSett", 0)
          set(out, , "abundanceSettledTemp", 0)
          out[(pixels %in% pixelsWithTooMany), cumsumAbSett := cumsum(abundanceSettled),
              by = c("pixels")]
          cumsumAboveSaturation <- which(out$cumsumAbSett > saturationDensity)
          abundanceSettledTemp <- pmax(0, saturationDensity -
                                         (out$cumsumAbSett[cumsumAboveSaturation] -
                                            out$abundanceSettled[cumsumAboveSaturation]))
          set(out, cumsumAboveSaturation, "abundanceActive", out$abundanceActive[cumsumAboveSaturation] +
                (out$abundanceSettled[cumsumAboveSaturation] - abundanceSettledTemp))
          set(out, cumsumAboveSaturation, "abundanceSettled", abundanceSettledTemp)
        }

        if (all(out[, sum(abundanceSettled, na.rm = TRUE) >= unique(Total), by = "initialPixels"])) {
          done <- TRUE
        }

        ## print debugging/status info
        print(paste(attr(out, "spreadState")$totalIterations,
                    paste(out[, sum(abundanceSettled, na.rm = TRUE) >= unique(Total),
                              by = "initialPixels"]$V1,
                    collapse = ", ")))
      }
    }
    aa <- raster(a)
    aa[] <- NA_integer_
    out1 <- out[, list(abundanceSettled = sum(abundanceSettled),
                       abundanceReceived = sum(abundanceReceived)), by = c("pixels")]
    aa[out1$pixels] <- out1$abundanceSettled
    #aa[out1$pixels] <- out1$abundanceReceived

    clearPlot()
    Plot(aa, zero.color = "red", new = TRUE, col = "Reds")
  }
})
