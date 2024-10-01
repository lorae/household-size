# generate-cpuma-state-crosswalk.R
#
# This script uses raw cpuma0010 data, present in the data/ipums_cpuma0010-sf
# folder, and processes it into a state crosswalk file.
#
# Why is this necessary? The cpuma0010 files already have a state crosswalk, but
# they also have geographies of each CPUMA saved, which is a quite computationally
# intensive file to parse, particularly when the shapefiles are not needed. As such,
# this script only retains the table that matches cPUMAs to states to allow easier
# aggregation.
#
# This script saves the crosswalk file to a database in data/db/helpers.duckdb
# with the table name "cpuma-state-cross"

# ----- Step 0: Load packages ----- #

library("sf")

# ----- Step 1: Load and process shapefile, remove geometry ----- #

cpuma0010_tb <- read_sf("data/ipums-cpuma0010-sf/ipums_cpuma0010.shp") |>
  st_drop_geometry()

# ----- Step 2: Save to DuckDB ----- #

con <- dbConnect(duckdb::duckdb(), "data/db/helpers.duckdb")
dbWriteTable(con, "cpuma-state-cross", cpuma0010_tb, overwrite = TRUE, temporary = FALSE)
DBI::dbDisconnect(con)

