#install_github("PredictiveEcology/SpaDES.tools@development")
library(data.table)
library(quickPlot)
library(raster)
library(SpaDES.tools)

dev.useRSGD(FALSE)
dev.new(useRSGD = FALSE)

# inputs
XMAX <- YMAX <- 10
a <- raster(extent(0, XMAX, 0, YMAX), res = 1)
pine <- gaussMap(a)
pine[] <- pine[] / maxValue(pine)
pine[] <- 1 ## TEMPORARY

LAMBDA <- 2

dispKern <- function(disFar, disNear, lambda) {
  (1 - exp(-lambda * disFar)) - (1 - exp(-lambda * disNear))
}

#dispKern1 <- function(dis, lambda) {
#  lambda * exp(-lambda * dis)
#}

loci <- c(ncell(a) / 3 + 1, ncell(a) * 2 / 3 + 1) ## get cell ids for attacked pixels
loci <- loci[1]

## max num red trees [= 1125 trees/ha * (MAPRES / 100)^2]
TOTAL <- round(1125 * (250 / 100)^2)

out <- spread2(a, start = loci, spreadProb = 1, asRaster = FALSE, iterations = 0,
               asymmetry = 10, asymmetryAngle = 90,
               circle = TRUE,
               returnDistances = TRUE, returnFrom = TRUE)
set(out, , "abundanceActive", TOTAL)
set(out, , "abundanceSettled", 0)

for (year in 2012:2016) {
  ## within a season, disperse the beetles
  done <- FALSE
  while (!done) {
    out <- spread2(a, start = out, spreadProb = 1, asRaster = FALSE, iterations = 1,
                   asymmetry = 10, asymmetryAngle = 90,
                   circle = TRUE,
                   returnDistances = TRUE, returnFrom = TRUE)
    set(out, , "order", seq_len(NROW(out)))
    attribs <- attr(out, "spreadState")
    
    #receivers <- out[!attr(out, "spreadState")$whActive, ]
    setkey(out, initialPixels, from)
    out[, beetleSource := !is.na(abundanceActive),  by = from]
    receivers <- out[beetleSource == FALSE]
    sources <- out[beetleSource == TRUE]
    
    setkey(sources, initialPixels, pixels)
    setkey(receivers, initialPixels, from)
    outW_i_columns <- receivers[sources]
    #outWLag1B <- outW_i_columns[which(state != "inactive")]
    outWLag1B <- outW_i_columns[beetleSource == FALSE]
    
    outWLag1B[, abundanceSettled :=
                pmin(TOTAL, round(pine[pixels] * dispKern(effectiveDistance, i.effectiveDistance, LAMBDA) *
                                    i.abundanceActive / (.N))), by = "from"]
    outWLag1B[, abundanceActive :=
                pmin(TOTAL, round((1 - pine[pixels] * dispKern(effectiveDistance, i.effectiveDistance, LAMBDA)) *
                                    i.abundanceActive / (.N))), by = "from"]
    set(outWLag1B, , grep(colnames(outWLag1B), pattern = "^i\\.", value = TRUE), NULL)
    
    out <- rbindlist(list(sources, outWLag1B), fill = TRUE)
    #out <- unique(out)
    setattr(out, "spreadState", attribs)
    if (sum(out[which(out$state != "inactive")]$abundanceSettled) == 0) {
      done <- TRUE
    }
    b <- raster(a)
    clearPlot()
    b[out[state == "activeSource", pixels]] <- 2
    b[out[state != "activeSource", pixels]] <- 1
    Plot(b)
  }
  out
  
  aa <- a
  aa[] <- NA_integer_
  aa[out$pixels] <- out$abundanceSettled
  
  plot(aa)
}
