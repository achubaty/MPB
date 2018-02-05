suppressPackageStartupMessages({
  library(amc)
  library(data.table)
  library(sp)
  library(quickPlot)
  library(reproducible)
  library(rgdal)
  library(raster)
  library(SpaDES.tools)
})

clearPlot()
dev.useRSGD(FALSE)
dev()

modulePath <- "~/GitHub/MPB/SpaDES/modules"

## spread
source(file.path(modulePath, "mpbRedTopSpread", "R", "insect_spread.R"))

# create a dummy raster
XMAX <- YMAX <- 100
aa <- randomPolygon(matrix(c(-115, 56), ncol = 2), hectares = 1)
crsString <- paste("+proj=lcc +lat_1=49 +lat_2=77 +lat_0=0 +lon_0=-95 +x_0=0 +y_0=0",
                   "+datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0")
aa <- spTransform(aa, CRS(crsString))
r <- raster(extent(aa), res = 1)

pine <- gaussMap(r)
pine <- mask(pine, aa)
pine[] <- pine[] / maxValue(pine)
pine <- setMinMax(pine)

pineVals <- sort(pine[], decreasing = TRUE)
loci <- which(pine[] >= pineVals[300])[sample(300, 150)]
# loci <- round(jitter(c(5025 + 0:15*100), factor = .2))
# loci <- c(loci, round(jitter(c(2050 + 0:15*100), factor = .2)))
# loci <- c(loci, round(jitter(c(8050 + 0:15*100), factor = .2)))

r2 <- insect_spread(pine,
                    loci = loci,
                    asymmetry = 5, asymmetryAngle = 78,
                    lambda = 0.12,
                    saturationDensity = 2632,
                    total = round(1125 * (250 / 100)^2),
                    debug = TRUE, maxIterations = 20)

# plot the result
clearPlot(force = TRUE)
#Plot(pine)
pine100 <- pine * 100
setColors(pine100, 50) <- rev(paste0(grey.colors(50), "55"))
setColors(r2, 100) <- paste0(colorRampPalette(RColorBrewer::brewer.pal(9, "Reds"))(100), "AA")
Plot(pine100, title = c("Example pine map\nwith MPB spreading through landscape"),
     legend = FALSE, visualSqueeze = 0.6)
Plot(r2, addTo = "pine100")
# lociMap <- raster(pine)
# lociMap[loci] <- 1
# Plot(lociMap, na.color = "transparent", addTo = "pine100")
# Plot(pine100, addTo = "r2", cols = "transparent")
