#install_github("PredictiveEcology/SpaDES.tools@prepInputs")
#devtools::load_all("~/GitHub/SpaDES.tools")
library(data.table)
library(quickPlot)
library(raster)
library(SpaDES.tools)

# dev.useRSGD(FALSE) 
# dev.new(useRSGD = FALSE) 

dev()

# inputs
XMAX <- YMAX <- 400
a <- raster(extent(0, XMAX, 0, YMAX), res = 1)
pine <- gaussMap(a)
pine[] <- pine[] / maxValue(pine) 
pine[] <- 0.6 ## TEMPORARY

# parameter in the dispKern function, describes steepness of curve, 
#  higher towards 1 is steeper, lower towards 0 is flatter
LAMBDA <- 0.12

dispKern <- function(disFar, disNear, lambda) {
  (1 - exp(-lambda * disFar)) - (1 - exp(-lambda * disNear))
}

#dispKern1 <- function(dis, lambda) {
#  lambda * exp(-lambda * dis)
#}

loci <- c(ncell(a) / 3 + 1, ncell(a) * 2 / 3 + 1) ## get cell ids for attacked pixels
loci <- loci[1]
loci <- sample(ncell(a), 100)

## max num red trees [= 1125 trees/ha * (MAPRES / 100)^2]
TOTAL <- round(1125 * (250 / 100)^2) 
saturationDensity <- 1000
asymm <- 5

out <- spread2(a, start = loci, spreadProb = 1, asRaster = FALSE, iterations = 0,
               asymmetry = asymm, asymmetryAngle = 0,
               circle = TRUE,
               returnDistances = TRUE, returnFrom = TRUE)
set(out, , "abundanceActive", 0)
set(out, , "abundanceSettled", 0)
set(out, , "abundanceReceived", 0)
set(out, , "Total", TOTAL)

for (year in 2012:2012) {
  ## within a season, disperse the beetles
  done <- FALSE
  while (!done) {
    nrowOut <- NROW(out)
    out <- spread2(a, start = out, spreadProb = 1, asRaster = FALSE, iterations = 1,
                   asymmetry = asymm, asymmetryAngle = 90,
                   circle = TRUE, allowOverlap = TRUE,
                   returnDistances = TRUE, returnFrom = TRUE)
    if (nrowOut != NROW(out)) {# might spread and have none that are <1 unit. Just go back to while loop
      set(out, , "order", seq_len(NROW(out)))
      attribs <- attr(out, "spreadState")
      
      #receivers <- out[!attr(out, "spreadState")$whActive, ]
      setkey(out, initialPixels, from)
      out[, beetleSource := !is.na(abundanceActive),  by = c("initialPixels", "from")]
      receivers <- out[beetleSource == FALSE]
      sources <- out[beetleSource == TRUE]
      
      setkey(sources, initialPixels, pixels)
      setkey(receivers, initialPixels, from)
      outW_i_columns <- receivers[sources]
      #outWLag1B <- outW_i_columns[which(state != "inactive")]
      outWLag1B <- outW_i_columns[beetleSource == FALSE]
      
      # Calculate the abundance received, as a function of distance
      outWLag1B[, abundanceReceived :=
                  pmin(i.Total, ceiling(dispKern(effectiveDistance, i.effectiveDistance, LAMBDA) *
                                          proportion *
                                          i.Total + i.abundanceActive *
                                          proportion / sum(proportion))),
                by = c("initialPixels", "from")] # Extra ones if they didn't settle
      # Calculate the abundance received, as a function of angle,
      # which was already calculated in spread2, and is called "proportion".
      # The pmin is about saturation density. 
      outWLag1B[, abundanceSettled := pmin(floor(abundanceReceived * pine[pixels] *
                                                   saturationDensity /
                                                   sum(abundanceReceived * pine[pixels])),
                                           ceiling(abundanceReceived * pine[pixels])),
                by = c("pixels")]
      # sources[, sum(abundanceSettled), by = c("initialPixels", "pixels")]
      outWLag1B[, abundanceActive := pmin(i.Total, floor(abundanceReceived - abundanceSettled))]#, by = "from"]
      
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
        set(out, , "rem", FALSE)
        out[(pixels %in% pixelsWithTooMany), `:=`(rem = cumsum(abundanceSettled) > saturationDensity), by = c("pixels")]
        out[which(rem), `:=`(abundanceActive=abundanceActive + abundanceSettled, abundanceSettled = 0)]  
      }
      
      if (all(out[, sum(abundanceSettled, na.rm = TRUE) >= unique(Total), by = "initialPixels"])) {
        done <- TRUE
      }
      # b <- raster(a)
      # clearPlot()
      # b[out[state == "activeSource", pixels]] <- 2
      # b[out[state != "activeSource", pixels]] <- 1
      # Plot(b)
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
  Plot(aa, zero.color = "red", new = TRUE)
}
