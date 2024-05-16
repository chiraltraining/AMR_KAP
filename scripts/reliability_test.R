# Install and load the 'psych' package
# https://m-clark.github.io/posts/2020-04-10-psych-explained/
library(tidyverse)
library(ltm)
library(psych)

# load data into R 
data <- readxl::read_excel("data/Final_Coded.xlsx")

# Select the relevant columns for the reliability test
knowledge <- dplyr::select(data, 12:23)
attitude <- dplyr::select(data, 26:35)
practices <- dplyr::select(data, 38:43)

# combined data 
selected_data <- cbind(knowledge, attitude, practices)

alpha_results <- psych::alpha(selected_data)

alpha_results$total
alpha_results$alpha.drop

# Suggested Actions to Enhance Cronbach's Alpha:
# 1. Explore Factor Structure (Factor Analysis)
factor_analysis <- factanal(covmat = cor(selected_data), factors = 1)
print(factor_analysis)

# 2. Check for Multidimensionality
# (Address potential subscales)
subscale_analysis <- psych::fa(selected_data)
print(subscale_analysis)


# Rerun Cronbach's alpha after modifications
enhanced_alpha_results <- psych::alpha(selected_data)
print(enhanced_alpha_results$total)
