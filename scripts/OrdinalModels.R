# Load packages 
library(tidyverse)
library(dplyr)
library(xlsx)
library(stringr)
library(gtsummary)
library(flextable)
library(janitor)
# factor reordering 
library(forcats)
# to perform model 
library(ordinal)
# to use polr 
library(MASS)

# Import data into 
data <- readxl::read_excel("data/AMR_KAP_Coded.xlsx")

# Clean names 
data <- data %>% 
  clean_names()

glimpse(data)

# Convert response variables into factor 
data$knowledge_level <- as.factor(data$knowledge_level)
data$attitude_level <- as.factor(data$attitude_level)
data$practice_level <- as.factor(data$practice_level)
data$practice_level_code <- as.factor(data$practice_level_code)

glimpse(data)

# Select variables 
df <- data %>% dplyr::select(1:9, knowledge_level, attitude_level, practice_level, practice_level_code)

# Check data structure 
glimpse(df)


# Factor Reorder 
df %>% 
  mutate_at(vars(knowledge_level), ~ fct_relevel(., c("Poor","Moderate", "Good"))) %>% 
  mutate_at(vars(attitude_level), ~ fct_relevel(., c("Negative","Uncertain","Positive"))) %>%
  mutate_at(vars(practice_level), ~ fct_relevel(., c("Misuse","Good")))


# Summary of response variable 
df %>% 
  dplyr::select(knowledge_level, attitude_level, practice_level) %>% 
  tbl_summary()

# Ordinal Logistic Regression for Knowledge 
model_knowledge <- clm(knowledge_level ~ 
      parent_s_age_years+ 
      parent_s_sex+
      parent_s_education_level+
      employment_status+
      family_type+ 
      family_type+your_average_household_income_per_month_bdt+
      child_s_sex+
      child_s_age_years+
      number_of_children,
    data = df)

report::report(model_knowledge)

summary(model_knowledge)
broom::tidy(model_knowledge)

reg_table1 <- model_knowledge %>% 
  tbl_regression(exponentiate = TRUE) %>% 
  bold_p(t = 0.05) %>% 
  bold_labels() %>% 
  as_flex_table()
reg_table1

save_as_docx(reg_table1, path = "tables/OLR_Knowledge.docx")

# Ordinal Logistic Regression for Attitude   
model_attitude <- clm(attitude_level ~ 
                         parent_s_age_years+ 
                         parent_s_sex+
                         parent_s_education_level+
                         employment_status+
                         family_type+ 
                         family_type+your_average_household_income_per_month_bdt+
                         child_s_sex+
                         child_s_age_years+
                         number_of_children,
                       data = df)

summary(model_attitude)
broom::tidy(model_attitude)

reg_table2 <- model_attitude %>% 
  tbl_regression(exponentiate = TRUE) %>% 
  bold_p(t = 0.05) %>% 
  bold_labels() %>% 
  as_flex_table()
