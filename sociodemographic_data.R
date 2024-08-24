# Define a function to calculate stats for a given column
calculate_stats <- function(data, column) {
  stats <- data %>%
    group_by({{ column }}) %>%
    tally()
  
  return(stats)
}


# Calculate and store the stats in a named list
stats_list <- list(
  gender_stats = calculate_stats(data_set, SD01),
  age_stats = calculate_stats(data_set, SD02_01),
  employment_status_stats = calculate_stats(data_set, SD03),
  highest_degree_stats = calculate_stats(data_set, SD04),
  german_knowledge_stats = calculate_stats(data_set, SD05),
  weekly_days_vocal = calculate_stats(data_set, KF01),
  weekly_days_instrumental = calculate_stats(data_set, KF02),
  frequency_work = calculate_stats(data_set, KF03),
  frequency_education = calculate_stats(data_set, KF04)
)

# Loop through the list and print each element
for (stat_name in names(stats_list)) {
  cat("\n---", stat_name, "---\n")
  print(stats_list[[stat_name]])
}