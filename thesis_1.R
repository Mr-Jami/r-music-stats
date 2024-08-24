# Step 1: Data Preparation
prepare_music_vs_no_music_data <- function(data) {
  data %>%
    mutate(ListeningStatus = ifelse(RD01 %in% c(2, 3), "Listens to Music", "Doesn't Listen to Music")) %>%
    filter(!is.na(TOTAL_SCORE)) %>%
    select(ListeningStatus, TOTAL_SCORE)
}

# Step 2: Perform Levene's Test
perform_levenes_test <- function(data) {
  levene_test_result <- leveneTest(TOTAL_SCORE ~ ListeningStatus, data = data)
  print(levene_test_result)
  use_welch <- levene_test_result$`Pr(>F)`[1] < 0.05
  return(use_welch)
}

# Step 3: Perform t-Test
perform_t_test <- function(data, use_welch) {
  music_scores <- data %>%
    filter(ListeningStatus == "Listens to Music") %>%
    pull(TOTAL_SCORE)
  
  no_music_scores <- data %>%
    filter(ListeningStatus == "Doesn't Listen to Music") %>%
    pull(TOTAL_SCORE)
  
  t_test_result <- t.test(music_scores, no_music_scores, alternative = "greater", var.equal = !use_welch)
  print(t_test_result)
}

# Step 4: Calculate Point-Biserial Correlation
calculate_music_correlation <- function(data) {
  data$MusicBinary <- ifelse(data$ListeningStatus == "Listens to Music", 1, 0)
  correlation_result <- cor(data$MusicBinary, data$TOTAL_SCORE)
  print(paste("Point-Biserial Correlation:", correlation_result))
}

# Step 5: Visualize Data
visualize_music_data <- function(data) {
  # Boxplot
  boxplot_total_score <- ggplot(data, aes(x = ListeningStatus, y = TOTAL_SCORE, fill = ListeningStatus)) +
    geom_boxplot() +
    labs(title = "Boxplot of TOTAL_SCORE by Listening Status",
         x = "Listening Status",
         y = "TOTAL_SCORE") +
    theme_minimal()
  print(boxplot_total_score)
  
  # Density Plot
  density_total_score <- ggplot(data, aes(x = TOTAL_SCORE, fill = ListeningStatus)) +
    geom_density(alpha = 0.5) +
    labs(title = "Density Plot of TOTAL_SCORE by Listening Status",
         x = "TOTAL_SCORE",
         y = "Density") +
    theme_minimal()
  print(density_total_score)
  
  # Qplot with geom_smooth
  qplot_total_score <- qplot(
    x = TOTAL_SCORE, 
    y = ListeningStatus, 
    data = data, 
    geom = c("point", "jitter"),
    main = "Scatter Plot with Smoothing of TOTAL_SCORE by Listening Status",
    xlab = "TOTAL_SCORE",
    ylab = "Listening Status"
  ) + 
    geom_smooth(method = "lm", aes(color = ListeningStatus), se = TRUE) +  # Add smoothing line
    theme_minimal()
  print(qplot_total_score)
}

# Main Execution
music_vs_no_music_data <- prepare_music_vs_no_music_data(data_set)
use_welch_for_t_test <- perform_levenes_test(music_vs_no_music_data)
perform_t_test(music_vs_no_music_data, use_welch_for_t_test)
calculate_music_correlation(music_vs_no_music_data)
visualize_music_data(music_vs_no_music_data)