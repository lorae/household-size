# create-synthetic-data.R
#
# This module contains helper functions that create synthetic IPUMS data

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
#'   \item{RACE}{Integer. A random value between 1 and 9 representing the race of 
#'     the individual.}
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
  
  n_na <- num_households * .1
  
  households <- tibble(
    SERIAL = 1:num_households,
    NUMPREC = sample(pphh_range["min"]:pphh_range["max"], num_households, replace = TRUE),
    HHWT = sample(hhwt_range["min"]:hhwt_range["max"], num_households, replace = TRUE),
    HHINCOME = sample(c(pmax(0, rnorm(num_households - n_na, mean = hhinc_mean, sd = hhinc_sd)), rep(9999999, n_na)))
  )
  
  create_person <- function(serial, num_persons) {
    tibble(
      SERIAL = serial,
      PERNUM = 1:num_persons,
      AGE = sample(age_range["min"]:age_range["max"], num_persons, replace = TRUE),
      SEX = sample(c(1, 2, 9), num_persons, replace = TRUE, prob = sex_probs),
      RACE = sample(1:9, num_persons, replace = TRUE)  # New column for RACE
    )
  }
  
  household_members <- map2(households$SERIAL, households$NUMPREC, create_person) |> bind_rows()
  
  household_members |> left_join(households, by = "SERIAL")
  
}



