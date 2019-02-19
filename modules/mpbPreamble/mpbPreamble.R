defineModule(sim, list(
  name = "mpbPreamble",
  description = "define MPB-specific study area for use with LandWeb",
  keywords = c("LandWeb"),
  authors = c(
    person(c("Eliot", "J", "B"), "McIntire", email = "eliot.mcintire@canada.ca", role = c("aut", "cre")),
    person(c("Alex", "M."), "Chubaty", email = "achubaty@friresearch.ca", role = c("aut"))
  ),
  childModules = character(0),
  version = list(SpaDES.core = "0.2.3.9009", mpbPreamble = "0.0.2"),
  spatialExtent = raster::extent(rep(NA_real_, 4)),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year",
  citation = list("citation.bib"),
  documentation = list("README.txt", "mpbPreamble.Rmd"),
  reqdPkgs = list("achubaty/amc", "fasterize", "magrittr", "maptools",
                  "PredictiveEcology/LandR@development",
                  "PredictiveEcology/map@development",
                  "PredictiveEcology/pemisc@development",
                  "raster", "RColorBrewer", "reproducible", "rgeos",
                  "sf", "sp", "SpaDES.tools"),
  parameters = rbind(
    defineParameter("minFRI", "numeric", 0, 0, 200, "The walue of fire return interval below which, pixels will be changed to NA, i.e., ignored"),
    defineParameter("runName", "character", NA, NA, NA, "A description for run; this will form the basis of cache path and output path"),
    defineParameter(".plotInitialTime", "numeric", NA, NA, NA, "This describes the simulation time at which the first plot event should occur"),
    defineParameter(".plotInterval", "numeric", NA, NA, NA, "This describes the simulation time interval between plot events"),
    defineParameter(".saveInitialTime", "numeric", NA, NA, NA, "This describes the simulation time at which the first save event should occur"),
    defineParameter(".saveInterval", "numeric", NA, NA, NA, "This describes the simulation time interval between save events"),
    defineParameter(".useCache", "logical", FALSE, NA, NA, "Should this entire module be run with caching activated? This is generally intended for data-type modules, where stochasticity and time are not relevant")
  ),
  inputObjects = bind_rows(
    expectsInput("canProvs", "SpatialPolygonsDataFrame",
                 desc = "Canadian provincial boundaries shapefile",
                 sourceURL = NA),
    expectsInput("studyAreaLarge", "SpatialPolygons",
                 desc = "The larger study area to use for spread parameter estimation.", ## TODO: better desc needed
                 sourceURL = NA)
  ),
  outputObjects = bind_rows(
    createsOutput("CC TSF", "RasterLayer", desc = NA), ## TODO: need descriptions for all outputs
    createsOutput("LandTypeCC", "RasterLayer", desc = NA),
    createsOutput("LCC2005", "RasterLayer", desc = NA),
    createsOutput("ml", "map", desc = NA),
    createsOutput("nonTreePixels", "integer", desc = NA),
    createsOutput("rasterToMatch", "RasterLayer", desc = NA),
    createsOutput("rasterToMatchReporting", "RasterLayer", desc = NA),
    createsOutput("rstFlammable", "RasterLayer", desc = NA),
    createsOutput("studyArea", "SpatialPolygonsDataFrame", desc = NA),
    createsOutput("studyAreaLarge", "SpatialPolygonsDataFrame", desc = NA),
    createsOutput("studyAreaReporting", "SpatialPolygonsDataFrame", desc = NA)
  )
))

doEvent.mpbPreamble = function(sim, eventTime, eventType) {
  switch(
    eventType,
    init = {
      sim <- Init(sim)
    },
    warning(paste("Undefined event type: '", current(sim)[1, "eventType", with = FALSE],
                  "' in module '", current(sim)[1, "moduleName", with = FALSE], "'", sep = ""))
  )
  return(invisible(sim))
}

Init <- function(sim) {
  cacheTags <- c(currentModule(sim), "function:.inputObjects")
  dPath <- asPath(getOption("reproducible.destinationPath", dataPath(sim)), 1)
  message(currentModule(sim), ": using dataPath '", dPath, "'.")

  ## LandWeb study area (we don't care about the LTHFCs; we need the polygons)
  fname <- file.path(dPath, "landweb_ltfc_v6.shp")
  landweb <- Cache(prepInputs,
                   targetFile = basename(fname),
                   alsoExtract = "similar",
                   archive = asPath(extension(fname, "zip")),
                   destinationPath = dPath,
                   url = "https://drive.google.com/open?id=1JptU0R7qsHOEAEkxybx5MGg650KC98c6",
                   fun = "raster::shapefile",
                   filename2 = NULL,
                   studyArea = fixErrors(spTransform(sim$studyAreaLarge, mod$prj)), ## TODO: why extra steps to fix?
                   targetCRS = mod$prj,
                   userTags = c("stable", currentModule(sim), "LandWebFRI")) %>%
    raster::intersect(., spTransform(sim$studyAreaLarge, mod$prj))

  mlLarge <- mapAdd(landweb, layerName = "MPB Study Area Large",
                    targetCRS = mod$prj, overwrite = TRUE,
                    columnNameForLabels = "NSN", isStudyArea = TRUE, filename2 = NULL)

  ##########################################################
  # LCC2005
  ##########################################################
  #  With full studyAreaLarge
  LCC2005 <- prepInputsLCC(studyArea = studyArea(mlLarge), destinationPath = Paths$inputPath)
  mlLarge <- mapAdd(LCC2005, layerName = "LCC2005", map = mlLarge, filename2 = NULL,
                    leaflet = FALSE, isRasterToMatch = TRUE, method = "ngb")

  # Put in smaller studyArea
  ml <- mapAdd(sim$studyArea, layerName = "MPB", useSAcrs = TRUE, poly = TRUE,
               analysisGroupReportingPolygon = "MPB", isStudyArea = TRUE,
               columnNameForLabels = "Name", filename2 = NULL)

  ##########################################################
  # LCC2005
  ##########################################################
  ml <- mapAdd(LCC2005, layerName = "LCC2005", map = ml,
               filename2 = NULL, leaflet = FALSE,
               isRasterToMatch = TRUE, method = "ngb")

  ##########################################################
  # Current Conditions
  ##########################################################
  ccURL <- "https://drive.google.com/file/d/1JnKeXrw0U9LmrZpixCDooIm62qiv4_G1/view?usp=sharing"
  LandTypeFileCC <- file.path(Paths$inputPath, "LandType1.tif")
  sim$LandTypeCC <- Cache(prepInputs, LandTypeFileCC, studyArea = studyArea(ml),
                          url = ccURL, method = "ngb",
                          rasterToMatch = rasterToMatch(ml), filename2 = NULL)

  ##########################################################
  # Non Tree pixels
  ##########################################################
  # Setting NA values
  # 3 is shrub, wetland, grassland -- no veg dynamics happen -- will burn in fire modules
  # 4 is water, rock, ice
  # 5 is no Data ... this is currently cropland -- will be treated as grassland for fires
  treeClassesCC <- c(0, 1, 2)
  nontreeClassesCC <- c(3, 4)
  treePixelsTF <- sim$LandTypeCC[] %in% treeClassesCC
  #nonTreePixels <- sim$LandTypeCC[] %in% nontreeClassesCC

  treeClassesLCC <- c(1:15, 34:35)
  treePixelsLCCTF <- ml$LCC2005[] %in% treeClassesLCC

  LandTypeCCNA <- is.na(sim$LandTypeCC[])
  noDataPixels <- LandTypeCCNA | sim$LandTypeCC[] == 5
  noDataPixelsLCC <- is.na(ml$LCC2005[]) | ml$LCC2005[] == 0

  treePixels <- which(treePixelsTF)
  treePixelsLCCTF[!noDataPixels] <- NA
  treePixelsLCC <- which(treePixelsLCCTF)

  treePixelsCombined <- unique(c(treePixels, treePixelsLCC))
  nonTreePixels <- seq(ncell(ml$LCC2005))
  nonTreePixels <- nonTreePixels[!nonTreePixels %in% treePixelsCombined]

  sim$nonTreePixels <- nonTreePixels

  # Update rasterToMatch layer with all trees
  ml[[ml@metadata[ml@metadata$rasterToMatch == 1, ]$layerName]][sim$nonTreePixels] <- NA

  fname_age <- "Age1.tif"
  TSFLayerName <- "CC TSF"
  ml <- mapAdd(map = ml, url = ccURL, layerName = TSFLayerName, CC = TRUE,
               tsf = file.path(Paths$inputPath, fname_age), analysisGroup1 = "CC",
               targetFile = fname_age, filename2 = NULL,
               useCache = TRUE, isRasterToMatch = FALSE,
               alsoExtract = "similar", leaflet = FALSE)
  ml[[TSFLayerName]][] <- as.integer(ml[[TSFLayerName]][])

  ########################################################################
  # Age from KNN
  ########################################################################

  standAgeMapFilename <- "NFI_MODIS250m_kNN_Structure_Stand_Age_v0.tif"
  standAgeMap <- Cache(prepInputs, #notOlderThan = Sys.time(),
                       targetFile = standAgeMapFilename,
                       archive = asPath(c("kNN-StructureStandVolume.tar",
                                          "NFI_MODIS250m_kNN_Structure_Stand_Age_v0.zip")),
                       destinationPath = Paths$inputPath,
                       url = "http://tree.pfc.forestry.ca/kNN-StructureStandVolume.tar",
                       fun = "raster::raster",
                       studyArea = studyArea(ml),
                       rasterToMatch = rasterToMatch(ml),
                       maskWithRTM = TRUE,
                       method = "bilinear",
                       datatype = "INT2U",
                       filename2 = NULL, overwrite = TRUE,
                       userTags = c("stable", currentModule(sim)))
  ml[[TSFLayerName]][noDataPixels] <- standAgeMap[noDataPixels]
  ml[[TSFLayerName]][sim$nonTreePixels] <- NA

  ##########################################################
  # Flammability and Fire Return Interval maps
  ##########################################################

  ## flammability map shouldn't be masked (no gaps!);
  #    NAs outside the buffered study & snow/rock/ice area
  #    the only values we want NA
  #    use the LCC flammability map to fill in NA / nodata values

  # No data class is 5 -- these will be filled in by LCC2005 layer
  # NA_ids <- which(is.na(sim$LandTypeCC[]) | sim$LandTypeCC[] == 5)
  # Only class 4 is considered non-flammable
  rstFlammableCC <- defineFlammable(sim$LandTypeCC, nonFlammClasses = 4,
                                    mask = NULL, filename2 = NULL)
  rstFlammableCC <- deratify(rstFlammableCC, complete = TRUE)

  #LandTypeFileLCC <- file.path(Paths$inputPath, "LCC2005_V1_4a.tif")
  # Only classes 36, 37, 38, 39 is considered non-flammable
  rstFlammableLCC <- defineFlammable(LCC2005, nonFlammClasses = 36:39, mask = NULL, filename2 = NULL)
  rstFlammableLCC <- deratify(rstFlammableLCC, complete = TRUE)

  #rstFlammableLCC <- Cache(prepInputs, LandTypeFileLCC, studyArea = studyArea(ml),
  #                         url = ccURL, method = "ngb",
  #                         rasterToMatch = rasterToMatch(ml), filename2 = NULL) %>%
  #  defineFlammable(., nonFlammClasses = 36:39, mask = NULL, filename2 = NULL)

  sim$rstFlammable <- rstFlammableCC
  sim$rstFlammable[LandTypeCCNA] <- rstFlammableLCC[LandTypeCCNA]
  sim$rstFlammable[] <- as.integer(sim$rstFlammable[])

  sim$studyArea <- studyArea(ml, 1) %>% spTransform(crs(mod$prj)) ## TODO: why need to force this?
  sim$studyAreaLarge <- studyArea(mlLarge, 1) %>% spTransform(crs(mod$prj)) ## TODO: why need to force this?
  sim$studyAreaReporting <- studyArea(ml, 1) %>% spTransform(crs(mod$prj)) ## TODO: why need to force this?
  sim$rasterToMatch <- rasterToMatch(ml)
  sim$rasterToMatchLarge <- rasterToMatch(mlLarge)
  crs(sim$rasterToMatch) <- crs(mod$prj) ## TODO: why need to force this?
  crs(sim$rasterToMatchLarge) <- crs(mod$prj) ## TODO: why need to force this?

  sim$fireReturnInterval <- mlLarge$fireReturnInterval # no NAing here because this needs only

  sim$LCC2005 <- ml$LCC2005

  sim[[TSFLayerName]] <- ml[[TSFLayerName]]

  sim$rasterToMatchReporting <- postProcess(rasterToMatch(ml),
                                            studyArea = studyArea(ml, 2),
                                            filename2 = NULL) # this is the small one
  crs(sim$rasterToMatchReporting) <- crs(mod$prj) ## TODO: why need to force this?

  sim$ml <- ml

  ## some assertions:
  testObjs <- c("studyArea", "studyAreaLarge", "studyAreaReporting",
                "rasterToMatch", "rasterToMatchReporting", "rasterToMatchLarge",
                TSFLayerName)
  lapply(testObjs, function(x) {
    if (is.null(sim[[x]]))
      stop("mpbPreamble: ", paste0("sim$", x, " returned NULL."), call. = FALSE)
  })
  ## end assertions

  return(invisible(sim))
}

.inputObjects <- function(sim) {
  cacheTags <- c(currentModule(sim), "function:.inputObjects")
  dPath <- asPath(getOption("reproducible.destinationPath", dataPath(sim)), 1)
  message(currentModule(sim), ": using dataPath '", dPath, "'.")

  mod$prj <- paste("+proj=lcc +lat_1=49 +lat_2=77 +lat_0=0 +lon_0=-95",
                   "+x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0")

  ## load study area
  if (!suppliedElsewhere("studyArea")) {
    sim$studyArea <- amc::loadStudyArea(dataPath(sim), "studyArea.kml", mod$prj) %>%
      spTransform(mod$prj)
    ## TODO: need to re-enforce the projection (even though it's done internally above)
  }

  ## provincial boundaries
  if (!suppliedElsewhere("canProvs", sim)) {
    sim$canProvs <- Cache(prepInputs, dlFun = "getData", "GADM", country = "CAN",
                          level = 1, path = dPath,
                          targetFile = "gadm36_CAN_1_sp.rds", ## TODO: this will change as GADM data update
                          fun = "base::readRDS") %>%
      spTransform(mod$prj)
  }

  ## studyAreaLarge
  if (!suppliedElsewhere("studyAreaLarge")) {
    west <- sim$canProvs[sim$canProvs$NAME_1 %in% c("Alberta", "Saskatchewan"), ]
    west <- Cache(postProcess, west, targetCRS = mod$prj, filename2 = NULL)

    sim$studyAreaLarge <- Cache(prepInputs,
                                targetFile = "NABoreal.shp",
                                alsoExtract = "similar",
                                archive = asPath("boreal.zip"),
                                destinationPath = dPath,
                                url = "http://cfs.nrcan.gc.ca/common/boreal.zip",
                                fun = "sf::read_sf",
                                useSAcrs = TRUE,
                                studyArea = west,
                                filename2 = NULL,
                                userTags = c("stable", currentModule(sim), "NorthAmericanBoreal")) %>%
      as("Spatial") %>%
      fixErrors(.)
  }

  return(sim)
}
