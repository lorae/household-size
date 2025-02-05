library(httr)
library(jsonlite)
library(dplyr)

# ----- Step 1: Get 2000 Census data ----- #
# API URL for 2000 Census (Decennial SF1)
url_2000 <- "https://api.census.gov/data/2000/dec/sf1?get=P001001,NAME&for=state:*"

# Send request and parse JSON response
response_2000 <- GET(url_2000)
data_2000 <- content(response_2000, as = "text")
pop_2000 <- fromJSON(data_2000)

# Convert to data frame and clean up
pop_2000 <- as.data.frame(pop_2000[-1,], stringsAsFactors = FALSE)
colnames(pop_2000) <- c("pop_2000", "state", "state_FIPS")
pop_2000$pop_2000 <- as.numeric(pop_2000$pop_2000)

# ----- Step 2: Get 2019 5-year ACS data ----- #
# API URL for 2019 ACS 5-Year Estimates
url_2019 <- "https://api.census.gov/data/2019/acs/acs5?get=B01003_001E,NAME&for=state:*"

# Send request and parse JSON response
response_2019 <- GET(url_2019)
data_2019 <- content(response_2019, as = "text")
pop_2019 <- fromJSON(data_2019)

# Convert to data frame and clean up
pop_2019 <- as.data.frame(pop_2019[-1,], stringsAsFactors = FALSE)
colnames(pop_2019) <- c("pop_2019", "state", "state_FIPS")
pop_2019$pop_2019 <- as.numeric(pop_2019$pop_2019)

# ----- Step 3: Merge the two tables, calculate population growth number and percent ----- $

pop_2000_2019 <- merge(
  pop_2000, pop_2019,
  by = c("state", "state_FIPS")
) |> 
  mutate(
    pop_growth = pop_2019 - pop_2000,
    pop_growth_percent = 100 * (pop_growth/pop_2000)
  )
