# Load packages 
library(tidyverse)
library(dplyr)
library(stringr)
library(gtsummary)
library(flextable)

# Load data into R 
data <- readxl::read_excel("data/AMR_KAP_Coded.xlsx")

# Demographic information 
df_demographic <- data %>% 
  select(1:11) 


# Data pre-processing for knowledge section 
df_knowledge <- data %>% 
  select(12:23) %>% 
  mutate(Total_Knowledge_Score = rowSums(across(where(is.numeric)))) %>% 
  mutate(Percent_Knowledge_Score = Total_Knowledge_Score) %>% 
  
  mutate(Knowledge_Level = case_when(
    Total_Knowledge_Score <= median(Total_Knowledge_Score) ~ 1, # 1 = Poor  
    Total_Knowledge_Score > median(Total_Knowledge_Score) ~ 2  # 2 = Good 
  ))

table(df_knowledge$Knowledge_Level)

  
# Data pre-processing for attitude section 
df_attitude <- data %>% 
  select(24:33) %>% 
  mutate(Total_Attitude_Score = rowSums(across(where(is.numeric)))) %>% 
  mutate(Attitude_Level = case_when(
    Total_Attitude_Score <= median(Total_Attitude_Score) ~ 1, # 1 = Negative
    Total_Attitude_Score > median(Total_Attitude_Score) ~ 2 # 2 = Positive
    
  ))


# Data pre-processing for practices section 
df_practices <- data %>% 
  select(34:39) %>% 
  mutate(Total_Practice_Score = rowSums(across(where(is.numeric)))) %>% 
  mutate(Practice_Level = case_when(
    Total_Practice_Score <= median(Total_Practice_Score) ~ 1, # 1 = Poor 
    Total_Practice_Score > median(Total_Practice_Score)  ~ 2  #2 = Good 
  ))


# Sources of information 
df_source_of_info <- data %>% 
  select(41:49) %>% 
  mutate(Others = `Others...49`)

# Reasons for self-medication
df_self_medication_reasons <- data %>% 
  select(51:62) %>% 
  mutate(Others = `Others...62`)


# Combined data 
processed_df <- cbind(df_demographic, df_knowledge, df_attitude, df_practices, df_source_of_info, df_self_medication_reasons)
# write_excel_csv(processed_df, file = "data/AMR_Processed_Data.csv")


# Level of KAP 
kap_level <- processed_df %>% 
  select(Knowledge_Level, Attitude_Level, Practice_Level) %>% 
  tbl_summary(missing = "no") %>% 
  as_flex_table() %>% 
  set_table_properties(width = 1, layout = "autofit")
  
kap_level 
# Export table 6
save_as_docx(kap_level, path = "tables/Table7.docx")



#-------------------------Model Building---------------------------
# Encoding Data 
processed_df$Knowledge_Level <- as.factor(processed_df$Knowledge_Level)
processed_df$Attitude_Level <- as.factor(processed_df$Attitude_Level)
processed_df$Practice_Level <- as.factor(processed_df$Practice_Level)
processed_df$`Parent’s age (years)` <- as.factor(processed_df$`Parent’s age (years)`)
processed_df$`Parent’s sex` <- as.factor(processed_df$`Parent’s sex`)
processed_df$`Parent’s education level` <- as.factor(processed_df$`Parent’s education level`)
processed_df$`Employment status` <- as.factor(processed_df$`Employment status`)
processed_df$`Family type` <- as.factor(processed_df$`Family type`)
processed_df$`Your average household income per month (BDT)` <- as.factor(processed_df$`Your average household income per month (BDT)`)
processed_df$`Number of children` <- as.factor(processed_df$`Number of children`)


glimpse(processed_df)

# Factors associated with better knowledge of the uses of antibiotics
model1 <- glm(Knowledge_Level ~ `Parent’s age (years)`+`Parent’s sex`+`Parent’s education level`+`Employment status`+
                 `Family type`+`Your average household income per month (BDT)`+`Number of children`, 
               data = processed_df, family = binomial)


Factors_Knowledge <- tbl_regression(model1, 
              exponentiate = TRUE, 
              pvalue_fun = ~style_pvalue(.x, digits = 2)) %>% 
              bold_p() %>%
              bold_labels() %>% 
              italicize_labels() %>% 
              as_flex_table() %>% 
              set_table_properties(width = 1, layout = "autofit")

Factors_Knowledge
# Export table 
save_as_docx(Factors_Knowledge, path = "tables/Factors_Knowledge.docx")


# Factors associated with positive attitude of the uses of antibiotics
model2 <- glm(Attitude_Level ~ `Parent’s age (years)`+`Parent’s sex`+`Parent’s education level`+`Employment status`+
                `Family type`+`Your average household income per month (BDT)`+`Number of children`, 
              data = processed_df, binomial(link = "logit"))

Factors_Attitude <- tbl_regression(model2, 
              exponentiate = TRUE, 
              pvalue_fun = ~style_pvalue(.x, digits = 2)) %>% 
              bold_p() %>%
              bold_labels() %>% 
              italicize_labels() %>% 
              as_flex_table() %>% 
              set_table_properties(width = 1, layout = "autofit")
Factors_Attitude
# Export table 
save_as_docx(Factors_Attitude, path = "tables/Factors_Attitude.docx")


# Factors associated with good practices of the uses of antibiotics
model3 <- glm(Practice_Level ~ `Parent’s age (years)`+`Parent’s sex`+`Parent’s education level`+`Employment status`+
                `Family type`+`Your average household income per month (BDT)`+`Number of children`, 
              data = processed_df, binomial(link = "logit"))

Factors_Practices <- tbl_regression(model3, 
              exponentiate = TRUE, 
              pvalue_fun = ~style_pvalue(.x, digits = 2)) %>% 
              bold_p() %>%
              bold_labels() %>% 
              italicize_labels() %>% 
              as_flex_table() %>% 
              set_table_properties(width = 1, layout = "autofit")

# Export table 
save_as_docx(Factors_Practices, path = "tables/Factors_Practices.docx")

