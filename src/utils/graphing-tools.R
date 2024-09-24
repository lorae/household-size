# Creates a simple black and white map of shapefiles. Useful for visualizing the number
# of CPUMAs in a region. Cannot
map_geographies <- function(data){
  ggplot(data) +
    geom_sf(fill = "white", color = "black") +
    theme_void()
}

