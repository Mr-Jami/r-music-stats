# set working directory
# Install and load packages
library(dplyr)
library(ggplot2)
library(car)
library(tidyr)

# Loading the data set of the survey
# Read the CSV file with the specified encoding
data_set <- read.csv2("files/dataset.csv", fileEncoding = "UTF-16")

# Remove the first data row after the header
data_set <- data_set[-1, ]

# Alternatively, if you prefer using dplyr, you can use the `slice` function:
# data_set <- data_set %>% slice(-1)

# Proceed with the rest of your script
source("calculate_sum.R")

# Calculate total score out of 29 possible points
data_set$TOTAL_SCORE <- data_set$AA03_SUM + data_set$AA05_SUM + data_set$AA08_SUM +
  data_set$AE22_SUM + data_set$AE23_SUM + data_set$AE26_SUM +
  data_set$AE04_01 + data_set$AE06_01 + data_set$AE08_01
data_set$TOTAL_SCORE_HARD <- data_set$AA03_SUM + data_set$AA05_SUM + data_set$AA08_SUM
data_set$TOTAL_SCORE_EASY <- data_set$TOTAL_SCORE - data_set$TOTAL_SCORE_HARD

source("sociodemographic_data.R")

# Write the modified dataset to a new CSV file without row names
write.csv2(data_set, "generated_files/new_dataset.csv", row.names = FALSE)

# Execute hypothesis scripts
cat("\n--- Hypothesis 1 ---\n")
source("thesis_1.R")
cat("\n--- Hypothesis 2 ---\n")
source("thesis_2.R")
cat("\n--- Hypothesis 3 ---\n")
source("thesis_3.R")
cat("\n--- Hypothesis 4 ---\n")
source("thesis_4.R")
