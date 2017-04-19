## ensure global parameters exist (from global.R)
stopifnot(exists(c("paths", "studyArea_txt")))

## -----------------------------------------------------------------------------
landisInputs <- readRDS(file.path(paths$inputPath, "landisInputs.rds"))
spEcoReg <- readRDS(file.path(paths$inputPath, "SpEcoReg.rds"))

seralStageData <- readRDS(file.path(paths$inputPath, "seralStageData.rds"))
vegTypeData <- readRDS(file.path(paths$inputPath, "vegTypeData.rds"))

loadShpAndMakeValid <- function(file) {
  shapefile(file) %>% gBuffer(byid = TRUE, width = 0)
}
shpStudyRegionFull <- Cache(loadShpAndMakeValid,
                            file = file.path(paths$inputPath,"shpLandWEB.shp"),
                            cacheRepo = paths$cachePath)
shpStudyRegionFull$fireReturnInterval <- shpStudyRegionFull$LTHRC
shpStudyRegionFull@data <- shpStudyRegionFull@data[,!(names(shpStudyRegionFull) %in% "ECODISTRIC")]

crs.knn <- CRS(paste("+proj=lcc +lat_1=49 +lat_2=77 +lat_0=0 +lon_0=-95",
                     "+x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"))

set.seed(853839)
shpStudyRegion <- if (studyArea_txt != "FULL") {
  if (studyArea_txt == "SMALL") {
    areaKm2 <- 10000
  } else if (studyArea_txt == "MEDIUM") {
    areaKm2 <- 40000
  } else if (studyArea_txt == "LARGE") {
    areaKm2 <- 80000
  }

  minY <- 7778877 - 1.6e5
  shpStudyRegionFull <- spTransform(shpStudyRegionFull, crs.knn)
  minX <- -1202250.2
  maxX <- minX + sqrt(areaKm2 * 1e6)
  maxY <- minY + sqrt(areaKm2 * 1e6)
  meanY <- mean(c(minY, maxY))

  ## Add random noise to polygon
  xAdd <- -3e5
  yAdd <- 5e5
  nPoints <- 20
  betaPar <- 0.6
  X <- c(jitter(sort(rbeta(nPoints, betaPar, betaPar) * (maxX - minX) + minX)),
         jitter(sort(rbeta(nPoints, betaPar, betaPar) * (maxX - minX) + minX, decreasing = TRUE)))
  Y <- c(jitter(sort(rbeta(nPoints / 2, betaPar, betaPar) * (maxY - meanY) + meanY)),
         jitter(sort(rbeta(nPoints, betaPar, betaPar) * (maxY - minY) + minY, decreasing = TRUE)),
         jitter(sort(rbeta(nPoints / 2, betaPar, betaPar) * (meanY - minY) + minY)))

  Sr1 <- Polygon(cbind(X + xAdd, Y + yAdd))
  Srs1 <- Polygons(list(Sr1), "s1")
  inputMapPolygon <- SpatialPolygons(list(Srs1), 1L)
  crs(inputMapPolygon) <- crs.knn
  raster::intersect(shpStudyRegionFull, inputMapPolygon)
} else {
  shpStudyRegionFull
}

ggStudyRegion <- ggvisFireReturnInterval(shpStudyRegion, shpStudyRegionFull)

## -----------------------------------------------------------------------------
message(brk(), "finished running inputMaps.R [", Sys.time(), "]", "\n", brk())
