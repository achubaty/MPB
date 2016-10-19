library(magrittr)
library(rgdal)
library(sp)
library(stringr)

# check that gdal driver exists to open gdb file
if (NROW(subset(ogrDrivers(), grepl("GDB", name))) == 0) {
  warning('The OpenFileGDB driver is not available on your system.')
}

##  file originally downlooaed from:
##    ftp://ftp.env.gov.ab.ca/pub/MPB/Data/MPB_AERIAL_SURVEY.gdb.7z
##
##  [ password protected archive, downloaded October 19, 2016                       ]
##  [ see email from Aaron McGill (Aaron.McGill@gov.ab.ca) forwarded by Barry Cooke ]
##
file <- '//W-VIC-A105388/shared/data/MPB/MPB_AERIAL_SURVEY.gdb'

# List all feature classes in a file geodatabase
fc_list <- ogrListLayers(file)
print(fc_list)

# each layer represents data for a single year (2 of the last 3 characters in the name)
years <- str_sub(fc_list, 10, 12)
years <- ifelse(years < 50, paste0(20, years), paste0(19, years))

# Read the feature class
layers <- structure( vector("list", length(years)) , names = years)
i <- 1
lapply(fc_list, function(x) {
  fc <- readOGR(dsn = file, layer = x)
  layers[[i]] <<- fc
  i <<- i + 1
})

# Determine the FC extent, projection, and attribute information
lapply(layers, summary)

# load boreal.west (intersection of boreal forest with AB, BC, SK)
load('//W-VIC-A105388/shared/data/MPB/Rmaps/boreal.west.RData')

# reproject boreal.west to match the projection of the mpb layer
boreal.west.2 <- spTransform(boreal.west, CRSobj = proj4string(layers[[1]]))

# plot them
dev.new(noRStudioGD = TRUE)
plot(boreal.west.2)
lapply(layers, plot, add = TRUE, col = 'darkred')
