library(httr)
library(jsonlite)
library(dplyr)

# Function to fetch Census data
fetch_census_data <- function(year, dataset, variable, col_name) {
  url <- paste0("https://api.census.gov/data/", year, "/", dataset, "?get=", variable, ",NAME&for=state:*")
  
  response <- GET(url)
  
  if (http_error(response)) {
    stop("Error fetching data from Census API for year ", year)
  }
  
  data <- content(response, as = "text") |> fromJSON()
  
  # Convert to tibble, clean up column names
  tibble(
    State = data[-1, 2],
    FIPS = data[-1, 3],
    !!col_name := as.numeric(data[-1, 1])
  )
}

# Fetch data
pop_2000 <- fetch_census_data(2000, "dec/sf1", "P001001", "pop_2000")
pop_2019 <- fetch_census_data(2019, "acs/acs5", "B01003_001E", "pop_2019")

# Merge and compute growth
state_pop_growth <- left_join(pop_2000, pop_2019, by = c("State", "FIPS")) |> 
  mutate(
    pop_growth = pop_2019 - pop_2000,
    pop_growth_percent = 100 * (pop_growth / pop_2000)
  )

# Save result
save(
  state_pop_growth,
  file = "data/helpers/state-pop-growth.rda"
)


