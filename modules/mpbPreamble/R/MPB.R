studyAreaMPB <- function(ml, runName, dataDir, canProvs) {
  if (!dir.exists(dataDir)) dir.create(dataDir)

  prj <- paste("+proj=lcc +lat_1=49 +lat_2=77 +lat_0=0 +lon_0=-95",
               "+x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")
  mpb <- amc::loadStudyArea(dataDir, "studyArea.kml", prj)

  shapefile(mpb, filename = file.path(dataDir, "studyArea.shp"), overwrite = TRUE)

  ## reportingPolygons
  ml <- mapAdd(mpb, ml, layerName = "MPB", useSAcrs = TRUE, poly = TRUE,
               analysisGroupReportingPolygon = "MPB", isStudyArea = TRUE,
               columnNameForLabels = "Name", filename2 = NULL)

  ## studyArea shouldn't use analysisGroup because it's not a reportingPolygon
  mpb_sr <- postProcess(ml$`MPB Study Area Large`,
                        studyArea = amc::outerBuffer(mpb, 50000), # 50 km buffer
                        useSAcrs = TRUE,
                        filename2 = file.path(dataDir, "MPB_SR.shp"),
                        overwrite = TRUE)
  #plot(mpb_sr)

  ml <- mapAdd(mpb_sr, ml, isStudyArea = TRUE, layerName = "MPB SR",
               useSAcrs = TRUE, poly = TRUE, studyArea = NULL, # don't crop/mask to studyArea(ml, 2)
               columnNameForLabels = "NSN", filename2 = NULL)

  return(ml)
}
