# create-summary-array.R
# 
# A module containing helper functions that convert tables of data to summary 
# arrays, as suggested by Galster (2024). 
# For full citation, please see the project README.

# Define dimension names
row_names <- c("Row1", "Row2", "Row3")
column_names <- c("Col1", "Col2", "Col3", "Col4")
layer_names <- c("Layer1", "Layer2")

# Create a 3D array with named elements for dimnames
named_array <- array(
  data = 1:24, 
  dim = c(3, 4, 2), 
  dimnames = list(
    rows = row_names, 
    columns = column_names, 
    layers = layer_names
  )
)

# Access row names using "rows" name
row_names <- dimnames(named_array)[["rows"]]
print(row_names)

# Access column names using "columns" name
column_names <- dimnames(named_array)[["columns"]]
print(column_names)

# Access layer names using "layers" name
layer_names <- dimnames(named_array)[["layers"]]
print(layer_names)