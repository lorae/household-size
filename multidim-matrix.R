# multidim-matrix.R
#
# This script produces a multidimensional aggregate matrix on synthetic data
# as a proof-of-concept for aggregation methods in the IPUMS data.

# ----- Step 0: Create synthetic data
# Set random seed for reproducibility
set.seed(42)

generate_household_data <- function(num_households = 40, 
                                    pphh_range = c(1, 5), # Persons per household range
                                    age_range = c(1, 100), 
                                    hhwt_range = c(50, 200), 
                                    sex_probs = c(0.45, 0.45, 0.1)) {
  # Create an empty data frame
  df <- data.frame(SERIAL = integer(),
                   AGE = integer(),
                   SEX = numeric(),
                   PERNUM = integer(),
                   HHWT = integer())
  
  # Generate data for each household
  for (serial in 1:num_households) {
    
    ## Generate household-level attributes
    # Number of persons in each household (random integer in pphh_range)
    num_persons <- sample(pphh_range[1]:pphh_range[2], 1)
    # Generate HHWT for the household within the specified range
    hhwt <- sample(hhwt_range[1]:hhwt_range[2], 1)
    
    ## Generate individual-level attributes within the household.
    pernums <- 1:num_persons
    ages <- sample(age_range[1]:age_range[2], num_persons, replace = TRUE)
    # Generate SEXes as 1, 2, or NA with specified probabilities for each person
    sexes <- sample(c(1, 2, NA), num_persons, replace = TRUE, prob = sex_probs)
    
    ## Create a temporary data frame for this household
    temp_df <- data.frame(SERIAL = rep(serial, num_persons),
                          AGE = ages,
                          SEX = sexes,
                          PERNUM = pernums,
                          HHWT = rep(hhwt, num_persons))
    
    # Bind to the main data frame
    df <- rbind(df, temp_df)
  }
  
  # Return the final data frame
  return(df)
}

# Example usage
household_data <- generate_household_data()
print(household_data)
