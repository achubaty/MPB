stopifnot(exists(c("crs.lflt", "globalRasters", "mySim", "polygons")))

## -----------------------------------------------------------------------------
function(input, output, session) {
  #react <- reactiveValues()
  seed <- sample(1e8,1)
  set.seed(seed)
  message("Current seed is: ", seed)

  ## currentPolygon is a reactiveVal object
  currentPolygon <- callModule(leafletMap, "leafletMap")

  ## do initial run of the model for the default study area
  if (TRUE) {
    ## Do initial run for each given study area so that all data prep done only once
    #initialRun1 <- spades(Copy(mySim), debug = TRUE)
    # 5 minutes for 6e3 km2
    # 30 minutes for 6e4 km2
    mySimCopy <- Copy(mySim)
    end(mySimCopy) <- start(mySimCopy)
    message("Running Initial spades call...")
    initialRun <- Cache(spades, sim = mySimCopy, #notOlderThan = Sys.time(),
                        debug = "paste(Sys.time(), paste(unname(current(sim)), collapse = ' '))",
                        .plotInitialTime = NA)

  }

  callModule(initialMap, "mpbMap", initialRun, "massAttacksMap")
  callModule(initialMap, "pineMap", initialRun, "pineMap")
  callModule(initialMap, "climateMap", initialRun, "climateSuitabilityMap")
  callModule(simOutputs, "simFigs", initialRun)
  #callModule(dataInfo, "modDataInfo", initialRun)
  callModule(simInfo, "simInfoTabs", initialRun)
  callModule(moduleInfo, "modInfoBoxes", initialRun)
  #callModule(moduleParams, "modParams", initialRun)

  # raster::endCluster()
  # message("Running Experiment")
  # args <- list(experiment, mySim, replicates = experimentReps,
  #              debug = "paste(Sys.time(), paste(unname(current(sim)), collapse = ' '),{lsObj <- ls(envir=sim@.envir); keep <- 1:1;
  #              a <- format(big.mark = ',',
  #                          sort(unlist(lapply(lsObj, function(x) object.size(get(x, envir=sim@.envir)))) %>% setNames(lsObj), decreasing = TRUE))[keep];
  #              paste(names(a)[keep], collapse=' ')},paste(a[keep], collapse=' '), 'NROW cohortData:', NROW(sim$cohortData), 'Num PixelGroups: ',
  #              uniqueN(sim$cohortData,by='pixelGroup'), 'PixelGroups:ncell:',
  #              uniqueN(sim$cohortData,by='pixelGroup')/ncell(sim$pixelGroupMap))",
  #              #debug = TRUE, #cache = TRUE,
  #              #cl = if(exists("cl")) cl,
  #              .plotInitialTime = NA,
  #              #notOlderThan = Sys.time(),
  #              clearSimEnv = TRUE)
  # args <- args[!unlist(lapply(args, is.null))]
  # mySimOut <- do.call(Cache, args)
  # message(attr(mySimOut, "tags"))
  # message(brk(), "  finished Experiment", Sys.time(), "]", "\n", brk())

  # filesFromOutputs <- lapply(seq_along(mySimOut), function(x) {
  #   outputs(mySimOut[[x]])$file
  # })
  #
  # for (simNum in seq_along(mySimOut)) {
  #   mySimOut[[simNum]]@outputs$file <- outputs(mySimOut[[simNum]])$file %>%
  #     strsplit(., split = paste0(outputPath(mySimOut[[simNum]]), "[\\/]+")) %>%
  #     lapply(., function(f) {
  #       f[[2]]
  #     }) %>%
  #     unlist() %>%
  #     file.path(paths$outputPath, .)
  # }
  #
  # rastersFromOutputs <- lapply(seq_along(mySimOut), function(x) {
  #   grep(pattern = "[.]grd$|[.]tif$", outputs(mySimOut[[x]])$file, value = TRUE)
  # })
  #
  # rastersFromOutputs <- unlist(rastersFromOutputs)
  # tsf <- grep(pattern = "rstTimeSinceFire", rastersFromOutputs, value = TRUE)
  # vtm <- grep(pattern = "vegTypeMap", rastersFromOutputs, value = TRUE)
  # lenTSF <- length(tsf)
  #
  # lfltFN <- gsub(tsf, pattern = "[.]grd$|[.]tif$", replacement = "LFLT.tif")
  #
  # if (!(length(globalRasters) == length(tsf))) {
  #   message("Reprojecting rasters & loading into RAM")
  #   globalRasters <<- lapply(seq_along(tsf), function(FN) {
  #     if (file.exists(lfltFN[FN])) {
  #       r <- raster(lfltFN[FN])
  #     } else {
  #       r <- raster(tsf[FN])
  #       r <- projectRaster(r, crs = crs.lflt, method = "ngb",
  #                          filename = lfltFN[FN], overwrite = TRUE,
  #                          datatype = "INT2U") ## is this the correct data type?
  #     }
  #     r
  #   })
  #   message("  Finished reprojecting rasters & loading into RAM")
  # }
}
