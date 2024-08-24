# Step 1: Data Preparation
prepare_music_task_data <- function(data) {
  data %>%
    filter(RD01 %in% c(2, 3)) %>%  # Only include music listeners
    filter(!is.na(TOTAL_SCORE_EASY), !is.na(TOTAL_SCORE_HARD)) %>%
    select(TOTAL_SCORE_EASY, TOTAL_SCORE_HARD)
}

# Step 2: Perform Levene's Test (compare variances between easy and hard tasks)
perform_levenes_test <- function(data) {
  combined_data <- data %>%
    gather(key = "TaskDifficulty", value = "Score", TOTAL_SCORE_EASY, TOTAL_SCORE_HARD) %>%
    mutate(TaskDifficulty = if_else(TaskDifficulty == "TOTAL_SCORE_EASY", "Easy", "Hard"))
  
  levene_test_result <- leveneTest(Score ~ TaskDifficulty, data = combined_data)
  print("Levene's Test for Task Difficulty:")
  print(levene_test_result)
  
  use_welch <- levene_test_result$`Pr(>F)`[1] < 0.05
  return(use_welch)
}

# Step 3: Perform Paired t-Test
perform_paired_t_test <- function(data, use_welch) {
  t_test_result <- t.test(data$TOTAL_SCORE_EASY, data$TOTAL_SCORE_HARD, 
                          alternative = "greater", paired = TRUE, var.equal = !use_welch)
  print("Paired t-Test for TOTAL_SCORE_EASY vs. TOTAL_SCORE_HARD:")
  print(t_test_result)
}

# Step 4: Calculate Correlation Between Easy and Hard Tasks
calculate_task_correlation <- function(data) {
  correlation_result <- cor(data$TOTAL_SCORE_EASY, data$TOTAL_SCORE_HARD)
  print(paste("Correlation Between TOTAL_SCORE_EASY and TOTAL_SCORE_HARD:", correlation_result))
}

# Step 5: Visualize Data
visualize_task_data <- function(data) {
  # Boxplot for Easy vs Hard Tasks
  combined_data <- data %>%
    gather(key = "TaskDifficulty", value = "Score", TOTAL_SCORE_EASY, TOTAL_SCORE_HARD) %>%
    mutate(TaskDifficulty = if_else(TaskDifficulty == "TOTAL_SCORE_EASY", "Easy", "Hard"))
  
  boxplot_task_scores <- ggplot(combined_data, aes(x = TaskDifficulty, y = Score, fill = TaskDifficulty)) +
    geom_boxplot() +
    labs(title = "Boxplot of Scores for Easy vs. Hard Tasks",
         x = "Task Difficulty",
         y = "Score") +
    theme_minimal()
  print(boxplot_task_scores)
  
  # Density Plot for Easy vs Hard Tasks
  density_task_scores <- ggplot(combined_data, aes(x = Score, fill = TaskDifficulty)) +
    geom_density(alpha = 0.5) +
    labs(title = "Density Plot of Scores for Easy vs. Hard Tasks",
         x = "Score",
         y = "Density") +
    theme_minimal()
  print(density_task_scores)
  
  # Scatter Plot with geom_smooth for Easy vs Hard Tasks
  scatter_plot_task_scores <- ggplot(data, aes(x = TOTAL_SCORE_EASY, y = TOTAL_SCORE_HARD)) +
    geom_point(alpha = 0.7) +
    geom_smooth(method = "lm", se = TRUE) +
    labs(title = "Scatter Plot of Easy vs. Hard Task Scores",
         x = "TOTAL_SCORE_EASY",
         y = "TOTAL_SCORE_HARD") +
    theme_minimal()
  print(scatter_plot_task_scores)
}

# Main Execution
music_task_data <- prepare_music_task_data(data_set)
use_welch_for_t_test <- perform_levenes_test(music_task_data)
perform_paired_t_test(music_task_data, use_welch_for_t_test)
calculate_task_correlation(music_task_data)
visualize_task_data(music_task_data)