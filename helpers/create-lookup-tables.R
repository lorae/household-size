# create-lookup-tables.R
#
# This script contains helper code to generate lookup tables for categorizing 
# various demographic variables (e.g., age, income) used in the main workflow.
# These lookup tables are saved as CSV files and can be reused or modified 
# for different bucketing schemes or sensitivity testing.
# 

# ----- Age lookup tables -----

# age_buckets00.csv
age_buckets00 <- data.frame(
  bucket_name = c("r00_49", "r50plus"),
  lower_bound = c(0, 50),
  upper_bound = c(50, 200)
)

write.csv(age_buckets00, "lookup_tables/age/age_buckets00.csv", row.names = FALSE)

# ----- Household income lookup tables -----

# hhincome_buckets00.csv
hhincome_buckets00 <- data.frame(
  bucket_name = c("LowIncome", "HighIncome"),
  lower_bound = c(0, 100000),  
  upper_bound = c(100000, Inf)
)

write.csv(hhincome_buckets, "lookup_tables/hhincome/hhincome_buckets00.csv", row.names = FALSE)

# hhincome_buckets01.csv
hhincome_buckets01 <- data.frame(
  bucket_name = c(
    "r000_020k", "r020k_040k", "r040k_060k", "r060k_080k", 
    "r080k_100k", "r100k_150k", "r150k_200k", "r200k_300k", "r300kplus"
  ),
  lower_bound = c(0, 20000, 40000, 60000, 80000, 100000, 150000, 200000, 300000),
  upper_bound = c(20000, 40000, 60000, 80000, 100000, 150000, 200000, 300000, Inf)
)

write.csv(hhincome_buckets, "lookup_tables/hhincome/hhincome_buckets01.csv", row.names = FALSE)
