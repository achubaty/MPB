## Libaries
library(sp)
library(raster)
library(RCurl)
library(rgdal)
library(rgeos)
library(snow)

## Set options
maps.dir = "~/Documents/Data/maps"
if(!file.exists(maps.dir)) stop("maps dir does not exist.")

download = FALSE
num.cpus = 4
rasterOptions(maxmemory=5e9)

## Study Region (process the entire conutry minus the territories)
getOGR <- function(layer, dir) {
  orig.dir = getwd()
  setwd(dir)
  out = readOGR(dsn=".", layer=layer)
  setwd(orig.dir)
  return(out)
}
boreal = getOGR("NABoreal", file.path(maps.dir, "boreal"))
boreal.can = boreal[boreal$COUNTRY=="CANADA",]
crs.boreal = CRS(proj4string(boreal))
study.region = c("British Columbia", "Alberta", "Saskatchewan", "Manitoba",
                 "Ontario", "Québec", "New Brunswick", "Nova Scotia",
                 "Newfoundland", "Prince Edward Island")

# provicial boundaries
load(file.path(maps.dir, "CAN_adm1.RData"))
canada1 = gadm
canada1.boreal = spTransform(canada1, crs.boreal)
SR.boreal = canada1.boreal[na.omit(match(study.region, canada1.boreal$NAME_1)),]
rm(gadm, canada1)

# study area (correct for non-adjacent boundaries)
SR.boreal.union.buff = gBuffer(SR.boreal, width=0.00001)
boreal.SR = intersect(SR.boreal.union.buff, boreal.can)
rm(SR.boreal.union.buff)

## Data directories
dem50k = file.path(maps.dir, "cded","50k_dem")
dem250k = file.path(maps.dir, "cded","250k_dem")
tmpdir = tmpDir()

## Fetch elevation data from internet
if (download) {
  ## Get files of size zero
  size.zero <- function(path=".", size=0) {
    files = dir(path, full.names=TRUE, recursive=TRUE)
    return(files[file.info(files)$size==size])
  }

  ## Define the ftp address
  geobase <- "ftp://ftp2.cits.rncan.gc.ca/pub/geobase/official/cded/"

  ## Download the documentation
  if (!file.exists(file.path(maps.dir, "doc")))
    dir.create(file.path(maps.dir, "doc"))

  fileNames <- unlist(strsplit(getURL(paste0(geobase, "doc/"),
                                      dirlistonly=TRUE), split="\n"))
  download.file(paste0(geobase, "doc/CDED.pdf"),
                file.path(maps.dir, "cded", "doc", "CDED.pdf"))
  download.file(paste0(geobase, "doc/GeoBase_product_specs_CDED1_en.pdf"),
                file.path(maps.dir, "cded", "doc",
                          "GeoBase_product_specs_CDED1_en.pdf"))
  download.file("http://www.geobase.ca/images/index/cded/250k/canada.jpg",
                file.path(maps.dir, "cded", "doc", "canada.jpg"))

  ## Download tiles of interest (ToI)
  ToI <- c(paste0("001", letters[11:14]),
           paste0("002", letters[3:6]),
           paste0("011", letters[c(4:7,10:12,15:16)]),
           paste0("012", letters[c(1:2,5:16)]),
           paste0("013", letters[1:15]),
           paste0("014", letters[c(3:6,12:13)]),
           paste0("020", letters[15:16]),
           paste0("021", letters[c(1:2,5,7:16)]),
           paste0("022", letters[1:16]),
           paste0("023", letters[1:16]),
           paste0("024", letters[c(1:14,16)]),
           paste0("025", letters[c(1:6)]),
           paste0("030", letters[c(12:14)]),
           paste0("031", letters[2:16]),
           paste0("032", letters[1:16]),
           paste0("033", letters[1:16]),
           paste0("034", letters[1:16]),
           paste0("035", letters[c(1:12)]),
           paste0("040", letters[c(7,9:10,15:16)]),
           paste0("041", letters[c(1,7:11,14:16)]),
           paste0("042", letters[1:16]),
           paste0("043", letters[c(1:8,10:15)]),
           paste0("044", letters[c(1,4,16)]),
           paste0("052", letters[1:16]),
           paste0("053", letters[1:16]),
           paste0("054", letters[c(1:8,11:13)]),
           paste0("055", letters[c(4:6,10:16)]),
           paste0("062", letters[5:16]),
           paste0("063", letters[1:16]),
           paste0("064", letters[1:16]),
           paste0("065", letters[1:16]),
           paste0("072", letters[5:16]),
           paste0("073", letters[1:16]),
           paste0("074", letters[1:16]),
           paste0("075", letters[1:16]),
           paste0("082", letters[5:16]),
           paste0("083", letters[1:16]),
           paste0("084", letters[1:16]),
           paste0("085", letters[1:16]),
           paste0("092", letters[c(2:3,5:12)]),
           paste0("093", letters[1:16]),
           paste0("094", letters[1:16]),
           paste0("095", letters[1:16]),
           paste0("102", c("i", "p")),
           paste0("103", letters[c(1:3,6:11)]),
           paste0("104", letters[c(1:3,6:12)]),
           paste0("105", letters[1:16]),
           paste0("115", letters[c(1:3,6:11,14:16)]))
  dirs = unique(substr(ToI, 1, 3))

  ## 250m
  invisible(lapply(file.path(dem250k, dirs), function(x) {
    if(!file.exists(x)) dir.create(x, recursive=TRUE) }))
  ToI.dl = substr(basename(list.files(file.path(dem250k), pattern="[.]zip$",
                                      recursive=TRUE)), 1, 4)
  zero = size.zero(file.path(dem250k), 0)
  redownload = sort(c(unlist(strsplit(setdiff(paste0(ToI, ".zip"), ToI.dl), "[.]zip$")),
                      unlist(strsplit(basename(zero), "[.]zip$"))))

  if (!is.null(redownload)) {
    for(i in redownload) {
      try(download.file(paste0(geobase, "250k_dem/", substr(i,1,3), "/", paste0(i, ".zip")),
                        file.path(dem250k, substr(i,1,3), paste0(i, ".zip"))))
    }
  }
  retry <- size.zero(file.path(dem250k), 0)
  if (length(retry)) {
    warning("The following 250k tiles did not download correctly:\n",
            paste("    ", basename(retry), collapse="\n"))
  }

  ## 50m
  invisible(lapply(file.path(dem50k, dirs), function(x) {
    if(!file.exists(x)) dir.create(x, recursive=TRUE) }))
  w <- 1
  while(w <= length(dirs)) {
    i <- dirs[w]
    # getURL: getting remote dir listings takes forever!
    # therefore, although we are getting a bit more data than we want,
    # it's much more efficient to download everything than be selective
    fn <- unlist(strsplit(getURL(paste0(geobase, "50k_dem/", i, "/"),
                                 dirlistonly=TRUE), split="\n"))

    if (exists("fn")) {
      ToI.dl = list.files(file.path(dem50k, i), pattern="[.]zip$")
      zero = size.zero(file.path(dem50k, i), 0)
      redownload = sort(c(setdiff(fn, ToI.dl), basename(zero)))
      sapply(redownload, function(x) {
        try(download.file(paste0(geobase, "50k_dem/", i, "/", x),
                          file.path(dem50k, i, x)))
        })
      rm(fn)
    }
    w <- w+1
  }
  retry <- size.zero(file.path(dem50k), 0)
  if (length(retry)) {
    warning("The following 50k tiles did not download correctly:\n",
            paste("    ", basename(retry), collapse="\n"))
  }
}

##------------------------------------------------------------------------------
##                                   250M_DEM
##------------------------------------------------------------------------------
## Unzip data
tmpdir250k = file.path(tmpdir, "250k_dem")
invisible(sapply(dir(file.path(dem250k), recursive=TRUE, pattern="[.]zip$",
                     full.names=TRUE), unzip, exdir=tmpdir250k))

## Note: dem(e) for East and dem(w) for West
dem <- lapply(dir(tmpdir250k, pattern="[.]dem$", full.names=TRUE), raster)
dem.all <- do.call(merge, dem)

beginCluster(num.cpus)
dem.all.boreal <- projectRaster(from=dem.all, crs=crs.boreal, method="bilinear")
endCluster()

elev.boreal <- clip.raster(dem.all.boreal, boreal.SR)

writeRaster(elev.boreal, file.path(maps.dir, "cded", "elevation_250k.tif"))

## Clean directory: keep only zip files
lapply(grep(dir(file.path(dem250k), full.names=TRUE),
            pattern="[.]zip$", value=TRUE, invert=TRUE), file.remove)

##------------------------------------------------------------------------------
##                                   50M_DEM
##------------------------------------------------------------------------------
## Unzip data
tmpdir50k = file.path(tmpdir, "50k_dem")
invisible(sapply(dir(file.path(dem50k), pattern="[.]zip$",
                     full.names=TRUE), unzip, exdir=file.path(dem50k)))

## Note: dem(e) for East and dem(w) for West
dem <- lapply(dir(file.path(dem50k), pattern="[.]dem$",
                  full.names=TRUE), raster)
dem.all <- do.call(merge, dem)

beginCluster(num.cpus)
dem.all.boreal <- projectRaster(from=dem.all, crs=crs.boreal, method="bilinear")
endCluster()

elev.boreal <- clip.raster(dem.all.boreal, boreal.SR)

writeRaster(elev.boreal, file.path(maps.dir, "cded", "elevation_50k.tif"))

## Clean directory: keep only zip files
lapply(grep(dir(file.path(dem50k), full.names=TRUE),
            pattern="[.]zip$", value=TRUE, invert=TRUE), file.remove)

##------------------------------------------------------------------------------
##
##------------------------------------------------------------------------------
srRt_100m <- disaggregate(srRt, fact=100)

dem_250k_100m <- projectRaster(from=dem_250k, to=srRt_100m)
dem_50k_100m <- projectRaster(from=dem_50k, to=srRt_100m)

sd_dem_250k <- aggregate(dem_250k_100m, fact=100, fun=sd)
sd_dem_50k <- aggregate(dem_50k_100m, fact=100, fun=sd)
