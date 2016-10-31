library(magrittr)
library(RCurl)
library(raster)
library(rasterVis)

os <- tolower(Sys.info()[["sysname"]])
user <- tolower(Sys.info()[["user"]])

# change working directory below as needed
if (user=="achubaty") {
  if (os=="darwin") {
    setwd("~/Documents/shared")
  } else if (os=="linux") {
    setwd("~/Documents/shared")
  } else if (os=="windows") {
    setwd("/shared")
  } else {
    stop("unable to automatically change working directory: ",
         "unknown operating system.")
  }
}

pineMap_dir <- "Yemshanov_pine_map"
if (!file.exists(pineMap_dir)) {
  dir.create(pineMap_dir)
}

# the paper describing the data set is here (behind a paywall):
#  http://link.springer.com/article/10.1007/s10661-011-2293-2
#
# but I have made it available via Dropbox:
#  https://www.dropbox.com/s/uh7g570gns1dpn3/yemshanov.etal_2012.pdf
#

## download files
eol <- ifelse(os=="windows", "\r\n", "\n")
url <- "ftp://ftp.nofc.cfs.nrcan.gc.ca/downloads/Cooke/Yemshanov_pine_map/"
paper_url <- "https://www.dropbox.com/s/uh7g570gns1dpn3/yemshanov.etal_2012.pdf"
paper <- basename(paper_url)
fileNames <- unlist(strsplit(getURL(url, dirlistonly=TRUE), split=eol))
lapply(fileNames, function(x) {
  if(!file.exists(file.path(pineMap_dir, x))) {
    download.file(paste0(url, x), destfile = file.path(pineMap_dir, x))
  }
})
if (!file.exists(file.path(pineMap_dir, paper))) {
   download.file(paper_url, destfile = file.path(pineMap_dir, paper))
}

## load the map
pineMap_file <- dir(pineMap_dir, pattern = "[.]flt$", full.names = TRUE)
pineMap_raster <- raster(pineMap_file)

## plot the map
# if the plot looks messed up it's likely due to corrupted download
dev.new(noRStudioGD = TRUE)
plot(pineMap_raster)

myTheme <- rasterTheme(region = brewer.pal('Greens', n = 9)[3:9])
levelplot(pineMap_raster, FUN.margin = median, maxpixels = 1e6, par.settings = myTheme)

