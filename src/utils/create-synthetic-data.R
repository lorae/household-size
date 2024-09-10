# create-synthetic-data.R
#
# This module contains helper functions that create synthetic IPUMS data

#' Generate Synthetic Household Data
#'
#' The `generate_household_data` function creates a synthetic dataset that mimics IPUMS-like household data, with realistic attributes for a specified number of households and persons per household.
#'
#' @param num_households Integer. The number of households to generate. Default is 40.
#' @param pphh_range Integer vector of length 2. Range for the number of persons per household (PERNUM). Default is c(1, 8), which means each household will have between 1 to 8 persons.
#' @param age_range Integer vector of length 2. Range for the age of persons in the households. Default is c(1, 100), which allows ages from 1 to 100 years.
#' @param hhwt_range Integer vector of length 2. Range for the household weight (HHWT). HHWT is a weight that applies to all individuals within a household. Default is c(50, 200).
#' @param sex_probs Numeric vector of length 3. Probabilities for assigning sex to individuals. The vector should have three elements, where the first element is the probability of assigning 1 (Male), the second is the probability of assigning 2 (Female), and the third is the probability of assigning NA (missing). Default is c(0.45, 0.45, 0.1).
#'
#' @return A data frame with synthetic household data containing the following columns:
#' \describe{
#'   \item{SERIAL}{Integer. Unique identifier for each household.}
#'   \item{AGE}{Integer. Age of the individual. Generated randomly within the specified age range.}
#'   \item{SEX}{Integer or NA. Sex of the individual. 1 represents Male, 2 represents Female, and NA represents missing data.}
#'   \item{PERNUM}{Integer. Person number within a household. For example, if a household has three people, the values would be 1, 2, 3.}
#'   \item{NUMPREC}{Integer. Number of persons in each household. This value is repeated for all persons within a household.}
#'   \item{HHWT}{Integer. Household weight, which is a random integer assigned to all persons within a household to represent the weight of the household in a survey context.}
#' }
#'
#' @examples
#' # Generate synthetic data with default settings
#' household_data <- generate_household_data()
#'
#' # Generate synthetic data with custom settings
#' household_data <- generate_household_data(
#'   num_households = 50,
#'   pphh_range = c(1, 6),
#'   age_range = c(0, 90),
#'   hhwt_range = c(30, 300),
#'   sex_probs = c(0.48, 0.48, 0.04)
#' )
#'
#' @export
generate_household_data <- function(num_households = 40, 
                                    pphh_range = c(1, 8), # Persons per household range
                                    age_range = c(1, 100), 
                                    hhwt_range = c(50, 200), 
                                    sex_probs = c(0.45, 0.45, 0.1)) {
  # Create an empty data frame
  df <- data.frame(SERIAL = integer(),
                   AGE = integer(),
                   SEX = numeric(),
                   PERNUM = integer(),
                   NUMPREC = integer(),
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
                          NUMPREC = rep(num_persons, num_persons),
                          HHWT = rep(hhwt, num_persons))
    
    # Bind to the main data frame
    df <- rbind(df, temp_df)
  }
  
  # Return the final data frame
  return(df)
}

