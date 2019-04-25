suppressPackageStartupMessages({
  library(ggplot2)
  library(magrittr)
  library(rgdal)
  library(sp)
  library(raster)
  library(rasterVis)
})

dev.useRSGD(FALSE)

outputDir <- file.path("outputs", "MPB_scfm")

studyArea <- shapefile("studyArea/studyArea.shp")

burnMaps <- file.path(outputDir, paste0("rstCurrentBurn_year", 2010:2030, ".tif"))
burnMapStack <- raster::stack(burnMaps)
gifName <- file.path(outputDir, "animation_burnMap.gif")
animation::saveGIF(ani.height = 1200, ani.width = 1200, interval = 1.0,
                   movie.name = gifName, expr = {
                     brks <- c(0, 0.5, 1)
                     cols <- c("grey", "red")
                     for (i in seq(numLayers(burnMapStack))) {
                       plot(burnMapStack[[i]], breaks = brks, col = cols)
                       #plot(spTransform(studyArea, crs(burnMapStack)), add = TRUE)
                     }
                   })
rm(burnMapStack)
