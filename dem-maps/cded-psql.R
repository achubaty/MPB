library(RPostgreSQL)

#-----
# Database info:
#
# Name    cded250k
# Type    PostgreSQL w/ PostGIS extensions enabled
# Server  W-VIC-A105342.pfc.forestry.ca
# Port    5433
#-----

### read the dem files into the database
#
# Based on:
#  https://gis.stackexchange.com/questions/18254/loading-a-raster-into-a-postgis-2-0-database-on-windows
#
# NOTE: this assumes that the data are unzippd and live in:
#       - the `tmp50k` directories specified in `cded-reprocess-50k.R`
#       - the `tmp250k` directories specified in `cded-reprocess-250k.R`
#
system(paste0("raster2pgsql -s 4269 -I -C -M ", file.path(tmpdir50k, "*.dem"),
              " public.cded50k | psql -d canadamaps "), intern=TRUE, wait=TRUE)

system(paste0("raster2pgsql -s 4269 -I -C -M ", file.path(tmpdir250k, "*.dem"),
              " public.cded250k | psql -d canadamaps "), intern=TRUE, wait=TRUE)

# open database connection
m <- dbDriver("PostgreSQL")
con <- dbConnect(m, dbname="canadamaps", host="localhost", port=5433,
                 user="gisuser", password="D!u2E*heeY34")

#
dbListTables(con)

# query database (could use dplyr directly I think)
q=""
rs = dbSendQuery(con, q)
df = fetch(rs, n=-1)

# close database connection
dbDisconnect(con)
rm(con, m)
