# This script is run to populate data/helpers/clean-state-cpuma-shapefiles.rda.
# The resulttant file can be read by other scripts to draw maps without repeating
# all these transformations and aesthetics.

# TODO: refactor this. Soem of the variable names are crappy and this could be a lot
# better.
# Maybe even combine this script with the one that creates the crosswalks of the cpuma
# to state

load("data/helpers/cpuma-state-cross.rda") # Crosswalks CPUMA0010 to state

rot <- function(a) {
  matrix(c(cos(a), sin(a), -sin(a), cos(a)), 2, 2)
}

# Moves a state according to custom specifications
transform_state <- function(
    df, 
    state_fp, 
    rotation_angle, 
    scale_factor, 
    shift_coords
) {
  state <- df %>% filter(STATEFIP == state_fp)
  state_geom <- st_geometry(state)
  state_centroid <- st_centroid(st_union(state_geom))
  rotated_geom <- (state_geom - state_centroid) * rot(rotation_angle * pi / 180) / scale_factor + state_centroid + shift_coords
  state %>% st_set_geometry(rotated_geom) %>% st_set_crs(st_crs(df))
}

# Create a list of states to loop through later
list_of_states <- cpuma_state_cross |>
  select(State) |>
  unique()

# Load shapefiles. Data is unzipped from https://www.weather.gov/gis/USStates
# Unfortunately, the Census shapefiles I tried to download all had strange shapes
# because they included water bodies.
state_sf <- st_read("data/s_05mr24/s_05mr24.shp") |>
  rename(STATEFIP = FIPS) |> # For consistency with household size data
  filter(!STATEFIP %in% c('60', '64', '66', '68', '69', '70', '72', '78')) |># Remove excluded states, like Puerto Rico
  st_transform(crs = "+proj=laea +lat_0=45 +lon_0=-100 +x_0=0 +y_0=0 +a=6370997 +b=6370997 +units=m +no_defs") |>
  mutate(geometry = st_simplify(geometry, dTolerance =  5000))  # Simplify shapes


# Rotate and move Alaska and Hawaii to fit on map
alaska <- transform_state(state_sf, "02", -39, 2.3, c(1000000, -5000000))
hawaii <- transform_state(state_sf, "15", -35, 1, c(5200000, -1400000))

# Final map after transforming non-contiguous states
state_sf_final <- state_sf |>
  filter(!STATEFIP %in% c("02", "15")) |>
  bind_rows(alaska, hawaii)

# Load shapefiles. Data is unzipped from WHERE? TODO: document
cpuma_sf <- st_read("data/ipums-cpuma0010-sf/ipums_cpuma0010.shp") |>
  filter(!STATEFIP %in% c('60', '64', '66', '68', '69', '70', '72', '78')) |># Remove excluded states, like Puerto Rico
  st_transform(crs = "+proj=laea +lat_0=45 +lon_0=-100 +x_0=0 +y_0=0 +a=6370997 +b=6370997 +units=m +no_defs") |>
  mutate(geometry = st_simplify(geometry, dTolerance =  5000))  # Simplify shapes

# Rotate and move Alaska and Hawaii to fit on map
alaska_cpuma <- transform_state(cpuma_sf, "02", -39, 2.3, c(1000000, -5000000))
hawaii_cpuma <- transform_state(cpuma_sf, "15", -35, 1, c(5200000, -1400000))

# Final map after transforming non-contiguous states
cpuma_sf_final <- cpuma_sf |>
  filter(!STATEFIP %in% c("02", "15")) |>
  bind_rows(alaska_cpuma, hawaii_cpuma)


# ----- SAVE EVERYTHING ----- #
cpuma_sf <- cpuma_sf_final
state_sf <- state_sf_final
save(cpuma_sf, state_sf, file = "data/helpers/state-cpuma-shapefiles.rda")


