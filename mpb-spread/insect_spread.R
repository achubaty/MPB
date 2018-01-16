#install_github("PredictiveEcology/SpaDES.tools@prepInputs")
#devtools::load_all("~/GitHub/SpaDES.tools")
library(data.table)
library(quickPlot)
library(raster)
library(SpaDES.tools)

dev()

# inputs
XMAX <- YMAX <- 100
a <- raster(extent(0, XMAX, 0, YMAX), res = 1)
pine <- gaussMap(a)
pine[] <- pine[] / maxValue(pine) 
#pine[] <- 1 ## TEMPORARY

LAMBDA <- 0.363

dispKern <- function(disFar, disNear, lambda) {
  (1 - exp(-lambda * disFar)) - (1 - exp(-lambda * disNear))
}

#dispKern1 <- function(dis, lambda) {
#  lambda * exp(-lambda * dis)
#}

loci <- c(ncell(a) / 3 + 1, ncell(a) * 2 / 3 + 1) ## get cell ids for attacked pixels
loci <- loci[1]
loci <- c(3334, 3339)
## max num red trees [= 1125 trees/ha * (MAPRES / 100)^2]
TOTAL <- round(1125 * (250 / 100)^2) 

out <- spread2(a, start = loci, spreadProb = 1, asRaster = FALSE, iterations = 0,
               asymmetry = 3, asymmetryAngle = 0,
               circle = TRUE,
               returnDistances = TRUE, returnFrom = TRUE)
set(out, , "abundanceActive", 0)
set(out, , "abundanceSettled", 0)
set(out, , "abundanceReceived", 0)

for (year in 2012:2012) {
  ## within a season, disperse the beetles
  done <- FALSE
  while (!done) {
    nrowOut <- NROW(out)
    out <- spread2(a, start = out, spreadProb = 1, asRaster = FALSE, iterations = 1,
                   asymmetry = 5, asymmetryAngle = 90,
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
                  pmin(TOTAL, ceiling(dispKern(effectiveDistance, i.effectiveDistance, LAMBDA) * proportion *
                                      (TOTAL + i.abundanceActive) ))]# Extra ones if they didn't settle
      
      # Calculate the abundance received, as a function of angle, which was already calculated in spread2, and is called "proportion"
      outWLag1B[, abundanceSettled := pmin(TOTAL, ceiling(abundanceReceived * pine[pixels]))]#, by = "from"]
      outWLag1B[, abundanceActive := pmin(TOTAL, floor(abundanceReceived - abundanceSettled))]#, by = "from"]
      
      set(outWLag1B, , grep(colnames(outWLag1B), pattern = "^i\\.", value = TRUE), NULL)
      
      out <- rbindlist(list(sources, outWLag1B), fill = TRUE)
      
      outSum <- out[, list(abundanceActive = sum(abundanceActive), abundanceSettled = sum(abundanceSettled), abundanceReceived = sum(abundanceReceived)), 
                    by = c("initialPixels", "pixels")]
      out <- unique(out, by = c("initialPixels", "pixels"))
      set(out, , "abundanceActive", outSum$abundanceActive)
      set(out, , "abundanceSettled", outSum$abundanceSettled)
      set(out, , "abundanceReceived", outSum$abundanceReceived)
      #out <- unique(out)
      setattr(out, "spreadState", attribs)
      if (out[, sum(abundanceSettled) >= TOTAL]) {
      #if (sum(out[which(out$state != "inactive")]$abundanceSettled) == 0) {
        done <- TRUE
      }
      # b <- raster(a)
      # clearPlot()
      # b[out[state == "activeSource", pixels]] <- 2
      # b[out[state != "activeSource", pixels]] <- 1
      # Plot(b)
      print(paste(attr(out, "spreadState")$totalIterations, out[, sum(abundanceSettled)]))
    }
  }
  aa <- raster(a)
  aa[] <- NA_integer_
  out1 <- out[, list(abundanceSettled=sum(abundanceSettled)), by = c("pixels")]
  aa[out1$pixels] <- out1$abundanceSettled
   
  plot(aa)
}
