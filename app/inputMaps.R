message(brk(), "start running inputMaps.R [", Sys.time(), "]", "\n")

## ensure global parameters exist (from global.R)
stopifnot(exists(c("paths")))

## -----------------------------------------------------------------------------
# load studyArea (SpatialPointsDataFrame)
f <- file.path(paths$inputPath, "studyArea", "studyArea.kml")
stopifnot(file.exists(f))
prj <- paste("+proj=aea +lat_1=47.5 +lat_2=54.5 +lat_0=0 +lon_0=-113 +x_0=0 +y_0=0",
             "+datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0")
studyArea <- readOGR(f, "studyArea.kml") %>% spTransform(., prj)
rm(f)

## ecodistricts etc. for leaflet maps ------------------------------------------
## downlooad ecodistricts etc. data
urls_eco <- list(
  ecozones = "http://sis.agr.gc.ca/cansis/nsdb/ecostrat/zone/ecozone_shp.zip",
  ecoprovinces = "http://sis.agr.gc.ca/cansis/nsdb/ecostrat/province/ecoprovince_shp.zip",
  ecoregions = "http://sis.agr.gc.ca/cansis/nsdb/ecostrat/region/ecoregion_shp.zip",
  ecodistricts = "http://sis.agr.gc.ca/cansis/nsdb/ecostrat/district/ecodistrict_shp.zip"
)

sapply(urls_eco, amc::dl.data, dest = paths$inputPath, unzip = TRUE)

## reproject/crop ecodistricts for studyArea
message("reprojecting ecodistricts...")
ecodistricts <- Cache(shapefile, file.path(paths$inputPath, "Ecodistricts", "ecodistricts"),
                      cacheRepo = paths$cachePath)
studyAreaEco <- spTransform(studyArea, crs(ecodistricts))
ecodistrictsStudyRegion <- Cache(crop, ecodistricts, studyAreaEco, cacheRepo = paths$cachePath)
ecodistricts <- spTransform(ecodistrictsStudyRegion, crs(studyArea))

## reproject/crop ecoregions for studyArea
message("reprojecting ecoregions...")
ecoregions <- Cache(shapefile, file.path(paths$inputPath, "Ecoregions", "ecoregions"),
                    cacheRepo = paths$cachePath)
ecoregionsStudyRegion <- Cache(crop, ecoregions, studyAreaEco, cacheRepo = paths$cachePath)
ecoregions <- spTransform(ecoregionsStudyRegion, crs(studyArea))

## reproject/crop ecoprovinces for studyArea
message("reprojecting ecoprovinces...")
ecoprovinces <- Cache(shapefile, file.path(paths$inputPath, "Ecoprovinces", "ecoprovinces"),
                      cacheRepo = paths$cachePath)
ecoprovincesStudyRegion <- Cache(crop, ecoprovinces, studyAreaEco, cacheRepo = paths$cachePath)
ecoprovinces <- spTransform(ecoprovincesStudyRegion, crs(studyArea))

## reproject/crop ecodistricts for studyArea
message("reprojecting ecozones...")
ecozones <- Cache(shapefile, file.path(paths$inputPath, "Ecozones", "ecozones"),
                      cacheRepo = paths$cachePath)
ecozonesStudyRegion <- Cache(crop, ecozones, studyAreaEco, cacheRepo = paths$cachePath)
ecozones <- spTransform(ecozonesStudyRegion, crs(studyArea))

message("reprojections complete!")

## create default study area for demo
demoArea <- ecodistricts[which(ecodistricts[["ECODISTRIC"]] == ._POLYNUM_.), ]
save(demoArea, file = file.path(paths$inputPath, paste0("demoArea_", ._POLYNUM_., ".rds")))

## create list of available polygons for leaflet -------------------------------
crs.lflt <- sp::CRS("+init=epsg:4326")
ecodistrictsLFLT <- spTransform(ecodistricts, crs.lflt)
ecoregionsLFLT <- spTransform(ecoregions, crs.lflt)
ecoprovincesLFLT <- spTransform(ecoprovinces, crs.lflt)
ecozonesLFLT <- spTransform(ecozones, crs.lflt)

availablePolygons <- names(urls_eco)
availablePolygonAdjective <- tools::toTitleCase(availablePolygons) %>%
  sapply(function(x) {
    substr(x, 1, nchar(x) - 1) ## trim the last letter
  }, USE.NAMES = FALSE)
availableProjections <- c("", "LFLT")
available <- data.frame(
  stringsAsFactors = FALSE,
  expand.grid(stringsAsFactors = FALSE,
              polygons = availablePolygons,
              projections = availableProjections),
  names = rep(tools::toTitleCase(availablePolygons), 2)
)

polygons <- lapply(seq_len(NROW(available)), function(ii) {
  get(paste0(available$polygons[ii], available$projections[ii]))
}) %>%
  setNames(available$names)

rm(list = c(names(urls_eco), paste0(names(urls_eco), "LFLT")))

polygonColours <- c(rep(c("red", "blue"), length(names(urls_eco))))
polygonIndivIdsColum <- list("ZONE_NAME", "PROVINCE_", "REGION_NAM", "ECODISTRIC") %>%
  set_names(names(polygons[1:4]))


##------------------------------------------------------------------------------

# timeSinceFirePalette <- leaflet::colorNumeric(
#   c(rep("red", 10), paste0(colorRampPalette(c("light green", "dark green"))(100), "FF")),
#   domain = NULL)
# attr(timeSinceFirePalette, "colorArgs")$na.color <- "#00000000"

# ggStudyRegion <- ggvisFireReturnInterval(studyArea, studyAreaFull)

## -----------------------------------------------------------------------------
message("finished running inputMaps.R [", Sys.time(), "]", "\n", brk())
