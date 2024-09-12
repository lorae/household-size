# create-lookup-tables.R
#
# This script contains helper code to generate lookup tables for categorizing 
# various demographic variables (e.g., age, income) used in the main workflow.
# These lookup tables are saved as CSV files and can be reused or modified 
# for different bucketing schemes or sensitivity testing.
#

# ----- Age lookup tables -----

# Define and populate the age lookup table row-wise
age_buckets00 <- rbind(
  c("r00_49", 0, 50, NA),
  c("r50plus", 50, 200, NA)
) %>%
  as.data.frame() %>%
  setNames(c("bucket_name", "lower_bound", "upper_bound", "specific_values")) %>% # Add column names
  type.convert(as.is = TRUE) # Convert column encoding to character or numeric

write.csv(age_buckets00, "lookup_tables/age/age_buckets00.csv", row.names = FALSE)

# ----- Household income lookup tables -----

# hhincome_buckets00.csv
hhincome_buckets00 <- rbind(
  c("NegIncome", -Inf, 0, NA),
  c("LowIncome", 0, 100000, NA),
  c("HighIncome", 100000, 9999999, NA),
  c("N/A", NA, NA, 9999999)
) %>%
  as.data.frame() %>%
  setNames(c("bucket_name", "lower_bound", "upper_bound", "specific_value")) %>% # Add column names
  type.convert(as.is = TRUE) # Convert column encoding to character or numeric

write.csv(hhincome_buckets00, "lookup_tables/hhincome/hhincome_buckets00.csv", row.names = FALSE)

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

write.csv(hhincome_buckets01, "lookup_tables/hhincome/hhincome_buckets01.csv", row.names = FALSE)

# ----- Ethnicity (hispanic) lookup tables -----

# hhincome_buckets00.csv
hispan_buckets00 <- rbind(
  c(0, "not_hispanic"),
  c(1, "hispanic"),
  c(2, "hispanic"),
  c(3, "hispanic"),
  c(4, "hispanic"),
  c(9, "N/A")
) %>%
  as.data.frame() %>%
  setNames(c("old_val", "new_val")) %>% # Add column names
  type.convert(as.is = TRUE) # Convert column encoding to character or numeric

write.csv(hispan_buckets00, "lookup_tables/hispan/hispan_buckets00.csv", row.names = FALSE)
