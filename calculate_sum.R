# Define a function to add sum columns and reorder them
add_sum_columns <- function(data, prefixes) {
  for (prefix in prefixes) {
    # Create a sum column name
    sum_column_name <- paste0(prefix, "_SUM")
    
    # Calculate the sum of the relevant columns
    data[[sum_column_name]] <- rowSums(data[ , grep(paste0("^", prefix, "_"), names(data))])
    
    # Reorder the columns to place the sum column after the last relevant column
    last_column <- paste0(prefix, "_04")  # Assuming "_04" is the last column for summing
    data <- data %>%
      select(1:which(names(data) == last_column),
             !!sum_column_name,
             everything())
  }
  return(data)
}

# Example usage with the data_set and prefixes
prefixes <- c("AA03", "AA05", "AA08", "AE22", "AE23", "AE26")
data_set <- add_sum_columns(data_set, prefixes)