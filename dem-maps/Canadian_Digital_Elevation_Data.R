## Libaries
library(raster)
library(RCurl)

maps.dir = "~/Documents/Data/maps/cded"
if(!file.exists(maps.dir)) stop("maps dir does not exist.")

rasterOptions(maxmemory=5e8, chunksize=5e7)

## Functions
clip.raster <- function(raster, shape) {
  a1_crop <- crop(raster, shape)
  step1 <- rasterize(shape, a1_crop)
  a1_crop * step1
}

## Data
srBnd <- shapefile(file.path(getwd(),"sr_extent_o.shp"))
srRt <- raster(file.path(getwd(),"eco_sr_o.tif")) ## Template
#srBnd <- shapefile(file.path(getwd(),"Study Region","Raster extent","sr_extent_o.shp"))
#srRt <- raster(file.path(getwd(),"Study Region","Raster extent","eco_sr_o.tif")) ## Template

## Fetch elevation data from internet
  ## Define the ftp address
  url <- "ftp://ftp2.cits.rncan.gc.ca/pub/geobase/official/cded/"

  ## Download the documentation
  if(!file.exists(file.path(maps.dir,"doc"))) dir.create(file.path(maps.dir,"doc"))

  fileNames <- unlist(strsplit(getURL(paste0(url, "doc/"), dirlistonly=TRUE), split="\n"))
  download.file(paste0(url,"doc/CDED.pdf"), file.path(maps.dir, "doc", "CDED.pdf"))
  download.file(paste0(url,"doc/GeoBase_product_specs_CDED1_en.pdf"), file.path(maps.dir,"doc","GeoBase_product_specs_CDED1_en.pdf"))

  ## Download tiles of interest
    ## 250m
    if(!file.exists(file.path(maps.dir,"250k_dem"))) dir.create(file.path(maps.dir,"250k_dem"))

    ToI <- c(paste0("001", c("k", "l", "m", "n")),
             paste0("002", c("c", "d", "e", "f")),
             paste0("011", letters[c(4:7,10:12,15:16)]),
             paste0("012", letters[c(1:2,5:16)]),
             paste0("013", letters[1:15]),
             paste0("014", letters[c(3:6,12:13)]),
             paste0("020", c("o", "p")),
             paste0("021", letters[c(1:2,5,7:16)]),
             paste0("022", letters[1:16]),
             paste0("023", letters[1:16]),
             paste0("024", letters[c(1:14,16)]),
             paste0("025", letters[c(1:5)]),
             paste0("030", letters[c(12:14)]),
             paste0("031", letters[c(2:16)]),
             paste0("032", letters[1:16]),
             paste0("033", letters[1:16]),
             paste0("034", letters[1:16]),
             paste0("035", letters[c(1:12)]),
             paste0("040", c("i", "j", "o", "p")),
             paste0("041", letters[c(1,7:11,14:16)]),
             paste0("042", letters[1:16]),
             paste0("043", letters[c(1:8,10:15)]),
             paste0("044", "d"),
             paste0("052", letters[1:16]),
             paste0("053", letters[1:16]),
             paste0("054", letters[1:16]),
             paste0("062", letters[5:16]),
             paste0("063", letters[1:16]),
             paste0("064", letters[1:16]),
             paste0("072", letters[5:16]),
             paste0("073", letters[1:16]),
             paste0("074", letters[1:16]),
             paste0("082", letters[5:16]),
             paste0("083", letters[1:16]),
             paste0("084", letters[1:16]),
             paste0("092", letters[c(2:3,5:12)]),
             paste0("093", letters[1:16]),
             paste0("094", letters[1:16]),
             paste0("102", c("i", "p")),
             paste0("103", letters[c(1:3,6:11)]),
             paste0("104", letters[c(1:3,6:12)]))

    for(i in ToI) {
      try(download.file(paste0(url, "250k_dem/", substr(i,1,3), "/", paste0(i,".zip")),
                    file.path(maps.dir, "250k_dem", paste0(i, ".zip"))))
    }
    retry <- basename(system(paste("find", file.path(maps.dir, "250k_dem"), "-size 0"), intern=TRUE))
    if (length(retry)) {
      warning("The following 250k tiles did not download correctly:\n",
              paste("    ", retry, collapse="\n"))
    }

    ## 50m
    if(!file.exists(file.path(maps.dir,"50k_dem"))) dir.create(file.path(maps.dir,"50k_dem"))

    w <- 1
    while(w <= length(ToI)){
      i <- ToI[w]
      fn <- unlist(strsplit(getURL(paste0(url, "50k_dem/", substr(i,1,3), "/"),
                                   dirlistonly = TRUE), split = "\n"))
      lapply(grep(fn, pattern=substr(i,1,4), value=TRUE), function(x) {
        try(download.file(paste0(url,"50k_dem/",substr(i,1,3),"/",x),
                      file.path(maps.dir,"50k_dem",x)))
        })
      w <- w+1
    }
    retry <- basename(system(paste("find", file.path(maps.dir, "50k_dem"), "-size 0"), intern=TRUE))
    if (length(retry)) {
      warning("The following 50k tiles did not download correctly:\n",
              paste("    ", retry, collapse="\n"))
    }


##------------------------------------------------------------------------------
##                                   250M_DEM
##------------------------------------------------------------------------------
## Unzip data
invisible(lapply(dir(file.path(getwd(),"Elevation","250k_dem"), pattern = "[.]zip$", full.names=TRUE), unzip, exdir = file.path(getwd(),"Elevation","250k_dem")))

## Note: dem(e) for East and dem(w) for West
dem <- lapply(dir(file.path(getwd(),"Elevation","250k_dem"), pattern = "[.]dem$", full.names = TRUE), raster)
demQC <- do.call(merge, dem)

dem_QClamb <- projectRaster(from = demQC, to = srRt, method="bilinear")

elevRt <- clip.raster(dem_QClamb, srBnd)

writeRaster(elevRt, file.path(getwd(),"Elevation","Elevation_250k.tif"))

## Clean directory: keep only zip files
lapply(grep(dir(file.path(getwd(),"Elevation","250k_dem"), full.names=TRUE), pattern = "[.]zip$", value = TRUE, invert = TRUE), file.remove)

##------------------------------------------------------------------------------
##                                   50M_DEM
##------------------------------------------------------------------------------
## Unzip data
invisible(lapply(dir(file.path(getwd(),"Elevation","50k_dem"), pattern = "[.]zip$", full.names=TRUE), unzip, exdir = file.path(getwd(),"Elevation","50k_dem")))

## Note: dem(e) for East and dem(w) for West
dem <- lapply(dir(file.path(getwd(),"Elevation","50k_dem"), pattern = "[.]dem$", full.names = TRUE), raster)
demQC <- do.call(merge, dem)

dem_QClamb <- projectRaster(from = demQC, to = srRt, method="bilinear")

elevRt <- clip.raster(dem_QClamb, srBnd)

writeRaster(elevRt, file.path(getwd(),"Elevation","Elevation_50k.tif"))

## Clean directory: keep only zip files
lapply(grep(dir(file.path(getwd(),"Elevation","50k_dem"), full.names=TRUE), pattern = "[.]zip$", value = TRUE, invert = TRUE), file.remove)



srRt_100m <- disaggregate(srRt, fact = 100)

dem_250k_100m <- projectRaster(from = dem_250k, to = srRt_100m)
dem_50k_100m <- projectRaster(from = dem_50k, to = srRt_100m)

sd_dem_250k <- aggregate(dem_250k_100m, fact=100, fun=sd)
sd_dem_50k <- aggregate(dem_50k_100m, fact=100, fun=sd)

