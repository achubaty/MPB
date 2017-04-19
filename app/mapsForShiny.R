## ensure global parameters exist (from global.R)
stopifnot(exists(c("paths")))

## -----------------------------------------------------------------------------
ecodistricts <- Cache(shapefile, file.path(paths$modulePath,"LW_LBMRDataPrep", "data", "ecodistricts"),
                      cacheRepo = paths$cachePath)
ecodistrictsFull <- Cache(shapefile, file.path(paths$modulePath,"LW_LBMRDataPrep", "data", "ecodistricts"),
                          cacheRepo = paths$cachePath)
shpStudyRegionEco <- spTransform(shpStudyRegion, crs(ecodistricts))
ecodistrictsStudyRegion <- Cache(crop, ecodistricts, shpStudyRegionEco, cacheRepo = paths$cachePath)
ecodistricts <- spTransform(ecodistrictsStudyRegion, crs(shpStudyRegion))

crs.lflt <- sp::CRS("+init=epsg:4326")

## available polygons
ecodistrictsDemoLFLT <- spTransform(ecodistricts, crs.lflt)
ecodistrictsFullLFLT <- spTransform(ecodistrictsFull, crs.lflt)
ecodistrictsDemo <- ecodistricts

availablePolygons <- c("ecodistricts")
availablePolygonAdjective <- c("Ecodistrict")
availableProjections <- c("", "LFLT")
availableScales <- c("Full", "Demo")
available <- data.frame(
  stringsAsFactors = FALSE,
  expand.grid(stringsAsFactors = FALSE, polygons = availablePolygons,
              scales = availableScales, projections = availableProjections),
  names = rep(c("Ecodistricts Full", "Ecodistricts Demo"), 2)
)

polygons <- lapply(seq_len(NROW(available)), function(ii) {
  get(paste0(available$polygons[ii], available$scales[ii], available$projections[ii]))
}) %>%
  setNames(available$names)

rm(ecodistrictsFull)
rm(ecodistrictsFullLFLT)

polygonColours <- c(rep(c("red", "blue"), 2))
polygonIndivIdsColum <- list("ECODISTRIC", "FMU_NAME") %>%
  setNames(names(polygons[1:(length(polygons) / 4) + (length(polygons) / 4) * 3]))

timeSinceFirePalette <- leaflet::colorNumeric(
  c(rep("red", 10), paste0(colorRampPalette(c("light green", "dark green"))(100), "FF")),
  domain = NULL)
attr(timeSinceFirePalette, "colorArgs")$na.color <- "#00000000"

## -----------------------------------------------------------------------------
message(brk(), "  finished running mapsForShiny.R [", Sys.time(), "]", "\n", brk())
