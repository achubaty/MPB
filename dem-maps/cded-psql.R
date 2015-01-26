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
# NOTE: this assumes that the data are unzippd and live in
#       the `tmp250k` directory specified in the
#
system(paste0("raster2pgsql -s 4269 -I -C -M ", file.path(tmpdir250k, "*.dem"),
              " | psql -d cded250k "), intern=TRUE, wait=TRUE)

# open database connection
m <- dbDriver("PostgreSQL")
con <- dbConnect(m, dbname="cded250k", host="localhost", port=5433,
                 user="achubaty", password="7EP*ppiFar0UT")

#
dbListTables(con)

# query database (could use dplyr directly I think)
q="SELECT ST_AsText(the_geom) AS geom from ccsm_polygons LIMIT 10;"
rs = dbSendQuery(con, q)
df = fetch(rs, n=-1)

# close database connection
dbDisconnect(con)
rm(con, m)
