# Sample Script for Sarah
#
# Using SQL queries for DuckDBs! Woah!

# ----- Step 0: Load required packages ----- #
library("magrittr")
library("dplyr")
library("duckdb")
# library("duckplyr")
library("dbplyr")
library("glue")

# ----- Step 1: Helper functions ----- #
#' Generate synthetic household data
#'
#' The `generate_household_data` function creates a synthetic dataset that mimics 
#' IPUMS-like household data, with realistic attributes for a specified number 
#' of households and persons per household.
#'
#' @param num_households Integer. The number of households to generate. Default 
#'   is 40.
#' @param pphh_range Integer vector of length 2, with named elements `min` and `max`. 
#'   Inclusive range for the number of persons per household (PERNUM). Default is 
#'   c(min = 1, max = 8), which means each household will have between 1 to 8 
#'   persons, inclusive.
#' @param age_range Integer vector of length 2, with named elements `min` and `max`. 
#'   Inclusive range for the age of persons in the households. Default is c(min = 0, 
#'   max = 100), which allows ages from 0 to 100 years, inclusive.
#' @param hhwt_range Integer vector of length 2, with named elements `min` and `max`. 
#'   Inclusive range for the household weight (HHWT). HHWT is a weight that applies 
#'   to all individuals within a household. Default is c(min = 50, max = 200).
#' @param sex_probs Numeric vector of length 3. Probabilities for assigning sex to 
#'   individuals. The vector should have three elements, where the first element 
#'   is the probability of assigning 1 (Male), the second is the probability of 
#'   assigning 2 (Female), and the third is the probability of assigning NA (missing). 
#'   Default is c(0.45, 0.45, 0.1).
#' @param hhinc_mean Numeric. The mean household income (HHINCOME) to be generated. 
#'   Default is 80,000.
#' @param hhinc_sd Numeric. The standard deviation of household income (HHINCOME). 
#'   Default is 40,000.
#'
#' @return A data frame with synthetic household data containing the following 
#'   columns:
#' \describe{
#'   \item{SERIAL}{Integer. Unique identifier for each household.}
#'   \item{AGE}{Integer. Age of the individual. Generated randomly within the 
#'     specified age range.}
#'   \item{SEX}{Integer or NA. Sex of the individual. 1 represents Male, 2 represents 
#'     Female, and NA represents missing data.}
#'   \item{PERNUM}{Integer. Person number within a household. For example, if a 
#'     household has three people, the values would be 1, 2, 3.}
#'   \item{NUMPREC}{Integer. Number of persons in each household. This value is 
#'     repeated for all persons within a household.}
#'   \item{HHWT}{Integer. Household weight, which is a random integer assigned to 
#'     all persons within a household to represent the weight of the household in 
#'     a survey context.}
#'   \item{HHINCOME}{Numeric. Household income, generated using a normal distribution 
#'     with a floor at 0. This value is the same for all members of a household.}
#' }
#'
#' @export
generate_household_data <- function(num_households = 40, 
                                    pphh_range = c(min = 1, max = 8), 
                                    age_range = c(min = 0, max = 100), 
                                    hhwt_range = c(min = 50, max = 200), 
                                    sex_probs = c(male = 0.45, female = 0.45, missing = 0.1),
                                    hhinc_mean = 80000,
                                    hhinc_sd = 40000) {
  
  # Input validation
  if (!all(c("min", "max") %in% names(pphh_range))) stop("pphh_range must have named elements 'min' and 'max'.")
  if (!all(c("min", "max") %in% names(age_range))) stop("age_range must have named elements 'min' and 'max'.")
  if (!all(c("min", "max") %in% names(hhwt_range))) stop("hhwt_range must have named elements 'min' and 'max'.")
  if (pphh_range["min"] > pphh_range["max"]) stop("Invalid pphh_range.")
  if (age_range["min"] > age_range["max"]) stop("Invalid age_range.")
  if (hhwt_range["min"] > hhwt_range["max"]) stop("Invalid hhwt_range.")
  if (length(sex_probs) != 3 || sum(sex_probs) != 1) stop("Invalid sex_probs.")
  if (num_households <= 0 || num_households %% 1 != 0) stop("num_households must be a positive integer.")
  if (hhinc_mean < 0) stop("hhinc_mean must be greater than or equal to 0.")
  
  # Generate data for each household
  households <- lapply(1:num_households, function(serial) {
    num_persons <- sample(pphh_range["min"]:pphh_range["max"], 1)
    hhwt <- sample(hhwt_range["min"]:hhwt_range["max"], 1)
    pernums <- 1:num_persons
    ages <- sample(age_range["min"]:age_range["max"], num_persons, replace = TRUE)
    sexes <- sample(c(1, 2, 9), num_persons, replace = TRUE, prob = sex_probs)
    # Generate HHINCOME (household income) with a normal distribution and a floor at 0
    # 10% of the time, the data is missing and assigned "9999999"
    hhincome <- ifelse(
      runif(1) <= 0.1,  # 10% chance
      9999999,  # Assign 9999999
      max(0, rnorm(1, mean = hhinc_mean, sd = hhinc_sd))
    )
    
    # Return household-level data as a data frame
    data.frame(SERIAL = rep(serial, num_persons),
               AGE = ages,
               SEX = sexes,
               PERNUM = pernums,
               NUMPREC = rep(num_persons, num_persons),
               HHWT = rep(hhwt, num_persons),
               HHINCOME = rep(hhincome, num_persons))
  })
  
  # Combine all households into a single data frame
  df <- do.call(rbind, households)
  
  return(df)
}




# Write the lookup tables to the connection
write_lookup_to_db <- function(
    con, # Name of the DuckDB connection
    data, # STRING: Name of the database containing data in the connection
    file_path # STRING: File path to the lookup table (must be CSV)
) {
  # Read the CSV lookup table
  lookup_table <- read.csv(file_path, stringsAsFactors = FALSE)
  
  # Write the lookup table to DuckDB
  copy_to(con, lookup_table, data, overwrite = TRUE)
}



# Create a query that assigns the buckets within the database
write_sql_query <- function(
    data, # STRING: Name of the database containing data in the connection
    lookup, # STRING: Name of the database containing the lookup table in the connection
    column_name # STRING: Name of the column being transformed
) {
  # Create a dynamic name for the bucket column
  bucket_column_name <- paste0(column_name, "_bucket")
  
  # Build the SQL query using glue
  sql_query <- glue::glue(
    "
    SELECT
        {data}.*,
        COALESCE(specific.bucket_name, range.bucket_name) AS {bucket_column_name}
    FROM
        {data}
    LEFT JOIN (
        SELECT *
        FROM {lookup}
        WHERE specific_value IS NOT NULL
    ) AS specific
        ON {data}.{column_name} = specific.specific_value
    LEFT JOIN (
        SELECT *
        FROM {lookup}
        WHERE specific_value IS NULL
    ) AS range
        ON {data}.{column_name} >= range.lower_bound
        AND {data}.{column_name} < range.upper_bound
  "
  )
  
  return(sql_query)  
}


# ----- Step 2: Build some lookup tables ----- #
# NOTE: This section will write 3 CSVs to your working directory.

# age_buckets00.csv
age_buckets00 <- rbind(
  c("r00_49", 0, 50, NA),
  c("r50plus", 50, 200, NA)
) %>%
  as.data.frame() %>%
  setNames(c("bucket_name", "lower_bound", "upper_bound", "specific_value")) %>% # Add column names
  type.convert(as.is = TRUE) # Convert column encoding to character or numeric

write.csv(age_buckets00, "age_buckets00.csv", row.names = FALSE)

# hhincome_buckets00.csv
hhincome_buckets00 <- rbind(
  c("negative", -Inf, 0, NA),
  c("r000_100k", 0, 100000, NA),
  c("r100kplus", 100000, 9999999, NA),
  c("N/A", NA, NA, 9999999)
) %>%
  as.data.frame() %>%
  setNames(c("bucket_name", "lower_bound", "upper_bound", "specific_value")) %>% # Add column names
  type.convert(as.is = TRUE) # Convert column encoding to character or numeric

write.csv(hhincome_buckets00, "hhincome_buckets00.csv", row.names = FALSE)

# hhincome_buckets01.csv
hhincome_buckets01 <- rbind(
  c("r000_020k", 0, 20000, NA),
  c("r020k_040k", 20000, 40000, NA),
  c("r040k_060k", 40000, 60000, NA),
  c("r060k_080k", 60000, 80000, NA),
  c("r080k_100k", 80000, 100000, NA),
  c("r100k_150k", 100000, 150000, NA),
  c("r150k_200k", 150000, 200000, NA),
  c("r200k_300k", 200000, 300000, NA),
  c("r300kplus", 300000, Inf, NA)
) %>%
  as.data.frame() %>%
  setNames(c("bucket_name", "lower_bound", "upper_bound", "specific_value")) %>% # Add column names
  type.convert(as.is = TRUE) # Convert column encoding to character or numeric

write.csv(hhincome_buckets01, "hhincome_buckets01.csv", row.names = FALSE)

# ----- Step 3: The fun stuff ----- #
# Set pseudorandom seed for replicability
set.seed(42)

# Generate synthetic household data
household_data <- generate_household_data()

# Connect to DuckDB
con <- dbConnect(duckdb::duckdb(), ":memory:")

# Write the IPUMS microdata table to the connection
dbWriteTable(con, "micro", household_data) # I'm naming it "micro" b/c it's microdata

# Buckets are defined in lookup tables that are stored as .csv files in the /lookup_tables/
# directory. There are several bucketing schemes saved. Here we explicitly choose
# each .csv file.
write_lookup_to_db(con, "age_lookup", "age_buckets00.csv")
write_lookup_to_db(con, "hhincome_lookup", "hhincome_buckets00.csv")

# Optional: print the tables to verify everything looks good
print(tbl(con, "age_lookup") %>% collect())
print(tbl(con, "micro") %>% head(n = 10) %>% collect())

# Apply the query to the database
micro_with_age_bucket <- tbl(
  con, 
  sql(
    write_sql_query( # Custom function for creating the SQL query using the lookup table
      data = "micro", 
      lookup = "age_lookup", 
      column_name = "AGE"
    )
  )
) %>%
  # The head(500) is optional but I use it to avoid loading in 14 million rows
  # in my actual dataset. Here it's unnecessary.
  head(500) %>%
  collect()

micro_with_hhincome_bucket <- tbl(
  con, 
  sql(
    write_sql_query( # Custom function for creating the SQL query using the lookup table
      data = "micro", 
      lookup = "hhincome_lookup", 
      column_name = "HHINCOME"
    )
  )
) %>%
  # The head(500) is optional but I use it to avoid loading in 14 million rows
  # in my actual dataset. Here it's unnecessary.
  head(500) %>%
  collect()

# ----- Step 4: The EXTRA fun stuff!! ----- #
# Let's look at the output!

View(micro_with_age_bucket)
View(micro_with_hhincome_bucket)
