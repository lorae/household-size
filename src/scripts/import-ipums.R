# import-ipums.R
#
# This script processes raw IPUMS data and saves it in a DuckDB file.
# 
# Current data: usa_00004 (2000, 2020). This file is .gitignored.
# TODO: replace manual data pull with an API call for reproducibility.
#
# For more on IPUMS and ipumsr: https://www.youtube.com/watch?v=OT6upQ1dBgU

# ----- Step 0: Load packages ----- #
library("dplyr")
library("duckdb")
library("ipumsr")

# ----- Step 1: Load and process IPUMS data ----- #

ddi <- read_ipums_ddi("data/ipums-microdata/usa_00010.xml")
ipums_tb <- read_ipums_micro(ddi, var_attrs = c()) 

# ----- Step 2: Save to DuckDB ----- #

con <- dbConnect(duckdb::duckdb(), "data/db/ipums-raw.duckdb")
dbWriteTable(con, "ipums", ipums_tb, overwrite = TRUE)
DBI::dbDisconnect(con)

# ----- Step 3: Save helpful reference documentation ----- #

# Interactive HTML document outlining the variables from the 
# data pull
ipums_view(ddi, out_file = "docs/ipums-data-dictionary.html", launch = FALSE)

# A list which provides the value labels for every variable
# in the pull. The tibble of value labels for a variable
# VARNAME, for example, can be accessed through  the following
# code:
# `my_tibble <- value_labels_list$VARNAME`

value_labels_list <- lapply(seq_len(nrow(ddi$var_info)), function(i) {
  val_labels <- ddi$var_info$val_labels[[i]]
  if (nrow(val_labels) > 0) {
    return(val_labels)
  } else {
    return(NULL)
  }
})

names(value_labels_list) <- ddi$var_info$var_name

save(value_labels_list, file = "docs/ipums_value_labels.RData")
