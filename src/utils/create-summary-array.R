# create-summary-array.R
# 
# A module containing helper functions that convert tables of data to summary 
# arrays, as suggested by Galster (2024). 
# For full citation, please see the project README.

# Function to create an empty array with named dimensions from individual vectors
create_named_array <- function(...) {
  # Capture all vectors passed as arguments
  dim_vectors <- list(...)
  
  # Get the length of each vector to determine the dimensions
  dim_lengths <- sapply(dim_vectors, length)
  
  # Get the names of the vectors to use as dimension names
  dim_names <- names(dim_vectors)
  
  # Create an empty array with named dimensions
  named_array <- array(
    data = NA,  # Initialize with NA (empty array)
    dim = dim_lengths,  # Set dimensions based on lengths of input vectors
    dimnames = dim_vectors  # Set dimension names based on input vectors
  )
  
  return(named_array)
}

# Use the function to initialize an empty array
test_array <- create_named_array(
  age = age_lookup_table[["bucket_name"]], 
  hhinc = hhinc_lookup_table[["bucket_name"]],
  sex = c(1,2,9)
  )

dimnames(test_array)

# Access age buckets using "age" dimension name
age_buckets <- dimnames(test_array)$age
print(age_buckets)

# Access hhinc buckets using "hhinc" dimension name
hhinc_buckets <- dimnames(test_array)$hhinc
print(hhinc_buckets)

# Access sex buckets "sex" dimension name
sex_buckets <- dimnames(test_array)$sex
print(sex_buckets)