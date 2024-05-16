# Install and load the pwr package if not already installed
if (!requireNamespace("pwr", quietly = TRUE)) {
  install.packages("pwr")
}

library(pwr)
library(tidyverse)

# load data into R 
data <- readxl::read_excel("data/Final_Coded.xlsx")

# level of knowledge 
data$Knowledge_Level <- as.factor(data$Knowledge_Level)

summary(data$Knowledge_Level)


# Replace 'variable_of_interest' with the actual variable you are interested in
# Separate data into two groups (e.g., treatment and control)
group_1 <- data$Knowledge_Level[data$Knowledge_Level == 1]
group_2 <- data$Knowledge_Level[data$Knowledge_Level == 2]
group_3 <- data$Knowledge_Level[data$Knowledge_Level == 3]

# Set parameters for the power analysis
effect_size <- 0.2  # Adjust based on your expected effect size
alpha <- 0.05       # Significance level
power <- 0.8        # Desired power
sample_size <- NULL  # This will be calculated by the power analysis

# Perform power analysis for a two-sample t-test
result <- pwr::pwr.t.test(d = effect_size, sig.level = alpha, power = power)

# Extract the required sample size from the result
required_sample_size <- ceiling(result$n)

# Print the result
cat("Required sample size for the specified power:", required_sample_size, "\n")
