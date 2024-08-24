# Step 1: Data Preparation
prepare_comparison_data <- function(data) {
  data %>%
    mutate(ListeningGroup = case_when(
      RD01 == 2 ~ "Instrumental Music", 
      RD01 == 1 ~ "No Music",
      RD01 == 3 ~ "Vocal Music"
    )) %>%
    filter(!is.na(TOTAL_SCORE), ListeningGroup %in% c("Instrumental Music", "No Music", "Vocal Music")) %>%
    select(ListeningGroup, TOTAL_SCORE)
}

# Step 2: Perform Levene's Test
perform_levenes_test <- function(data) {
  levene_test_result <- leveneTest(TOTAL_SCORE ~ ListeningGroup, data = data)
  print(levene_test_result)
  use_welch <- levene_test_result$`Pr(>F)`[1] < 0.05
  return(use_welch)
}

# Step 3: Perform t-Test
perform_t_test <- function(data, use_welch) {
  instrumental_scores <- data %>%
    filter(ListeningGroup == "Instrumental Music") %>%
    pull(TOTAL_SCORE)
  
  other_scores <- data %>%
    filter(ListeningGroup %in% c("No Music", "Vocal Music")) %>%
    pull(TOTAL_SCORE)
  
  t_test_result <- t.test(instrumental_scores, other_scores, alternative = "greater", var.equal = !use_welch)
  print(t_test_result)
}

# Step 4: Calculate Point-Biserial Correlation
calculate_point_biserial <- function(data) {
  # Convert the group comparison to binary: 1 = Instrumental Music, 0 = No Music or Vocal Music
  data$IsInstrumental <- ifelse(data$ListeningGroup == "Instrumental Music", 1, 0)
  correlation_result <- cor(data$IsInstrumental, data$TOTAL_SCORE)
  print(paste("Point-Biserial Correlation:", correlation_result))
}

# Step 5: Visualize Data
visualize_comparison_data <- function(data) {
  # Boxplot
  boxplot_total_score <- ggplot(data, aes(x = ListeningGroup, y = TOTAL_SCORE, fill = ListeningGroup)) +
    geom_boxplot() +
    labs(title = "Boxplot of TOTAL_SCORE by Listening Group",
         x = "Listening Group",
         y = "TOTAL_SCORE") +
    theme_minimal()
  print(boxplot_total_score)
  
  # Density Plot
  density_total_score <- ggplot(data, aes(x = TOTAL_SCORE, fill = ListeningGroup)) +
    geom_density(alpha = 0.5) +
    labs(title = "Density Plot of TOTAL_SCORE by Listening Group",
         x = "TOTAL_SCORE",
         y = "Density") +
    theme_minimal()
  print(density_total_score)
  
  # Qplot with geom_smooth
  qplot_total_score <- qplot(
    x = TOTAL_SCORE, 
    y = ListeningGroup, 
    data = data, 
    geom = c("point", "jitter"),
    main = "Scatter Plot with Smoothing of TOTAL_SCORE by Listening Group",
    xlab = "TOTAL_SCORE",
    ylab = "Listening Group"
  ) + 
    geom_smooth(method = "lm", aes(color = ListeningGroup), se = TRUE) +  # Add smoothing line
    theme_minimal()
  print(qplot_total_score)
}

# Main Execution
comparison_data <- prepare_comparison_data(data_set)
use_welch_for_t_test <- perform_levenes_test(comparison_data)
perform_t_test(comparison_data, use_welch_for_t_test)
calculate_point_biserial(comparison_data)
visualize_comparison_data(comparison_data)