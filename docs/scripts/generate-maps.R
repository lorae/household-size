# generate-maps.R
# Generates example images of CPUMA0010 regions for use in root/README.md

library("ggplot2")
library("sf")
library("svglite")

source("src/utils/graphing-tools.R")

# Read in shapefiles of every CPUMA0010 region in the United States
sf <- read_sf("ipums_cpuma0010/ipums_cpuma0010.shp")

# Minnesota CPUMA map
minnesota_map <- map_geographies(sf |> filter(State == "Minnesota"))
ggsave("docs/images/minnesota_cpumas.svg", plot = minnesota_map, width = 1200, height = 1200, units = "px")