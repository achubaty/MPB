library(chron)
library(ncdf4)

if (Sys.info()[["sysname"]]=="Windows") {
  path <- file.path("//W-VIC-A105254/shared/data/climate/CANESM2/NetCDF_files")
} else if (Sys.info()[["sysname"]]=="Linux") {
  path <- file.path("/mnt/A105254/shared/data/climate/CANESM2/NetCDF_files")
}
setwd(path)

dirs <- dir(path)
for (d in dirs) {
  ncFiles <- dir(file.path(path, d), pattern="[.]nc$", full.names=TRUE)
  
  for (f in ncFiles) {
    nc <- nc_open(f)
    
    print(nc)
    
    latitude <- ncvar_get(nc, varid="latitude")
    longitude <- ncvar_get(nc, varid="longitude")
    
    time <- ncvar_get(nc, varid="time")
    t <- chron(time, origin=c(month=12, day=31, year=1951))
    
    vals <- ncvar_get(nc, varid="swind_anom") ###
    
    image(longitude, rev(latitude), vals[, ncol(vals):1, 660]) #
    
    nc_close(f)
  }  
}
