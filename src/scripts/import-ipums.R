# import-ipums.R
#
# This script processes raw IPUMS data and saves it in a DuckDB file.
# 
# Current data: usa_00004 (2000, 2020). This file is .gitignored.
# TODO: replace manual data pull with an API call for reproducibility.
#
# For more on IPUMS and ipumsr: https://www.youtube.com/watch?v=OT6upQ1dBgU
#
# According to the Census Bureau: "A combination of SAMPLE and SERIAL provides a unique 
# identifier for every household in the IPUMS; the combination of SAMPLE, SERIAL, 
# and PERNUM uniquely identifies every person in the database."

# ----- Step 0: Load packages ----- #
library("dplyr")
library("duckdb")
library("ipumsr")

# ----- Step 1: Load and process IPUMS data ----- #

ddi <- read_ipums_ddi("usa_00004.xml")
ipums_tb <- read_ipums_micro(ddi, var_attrs = c()) 

# ----- Step 2: Save to DuckDB ----- #

con <- dbConnect(duckdb::duckdb(), "db/ipums.duckdb")
dbWriteTable(con, "ipums", ipums_tb, overwrite = TRUE)
DBI::dbDisconnect(con)

