# Load packages 
library(tidyverse)
library(gtsummary)
library(flextable)
library(officer)
library(likert)

# Descriptive tables 
# Load data into R 
ds <- readxl::read_excel("data/AMR_KAP_No_Code.xlsx")


# Table 1: Demographic characteristics 
table1 <- ds %>% 
  dplyr::select(1:11) %>% 
  tbl_summary(missing = "no", 
              type = everything() ~ "categorical") %>% 
  as_flex_table() %>% 
  set_table_properties(width = 1, layout = "autofit")

table1
# Export table 1 
save_as_docx(table1, path = "tables/Table1.docx")



# Table 2: Knowledge  
table2 <- ds %>% 
  dplyr::select(12:23) %>% 
  tbl_summary(missing = "no", 
              type = everything() ~ "categorical") %>% 
  as_flex_table() %>% 
  set_table_properties(width = 1, layout = "autofit")

table2
# Export table 2 
save_as_docx(table2, path = "tables/Table2.docx")


# Table 3: Attitude  
table3 <- ds %>% 
  dplyr::select(24:33) %>% 
  tbl_summary(missing = "no", 
              type = everything() ~ "categorical") %>% 
  as_flex_table() %>% 
  set_table_properties(width = 1, layout = "autofit")

table3
# Export table 3
save_as_docx(table3, path = "tables/Table3.docx")


# Table 4: Practices   
table4 <- ds %>% 
  dplyr::select(34:39) %>% 
  tbl_summary(missing = "no", 
              type = everything() ~ "categorical") %>% 
  as_flex_table() %>% 
  set_table_properties(width = 1, layout = "autofit")

table4
# Export table 4 
save_as_docx(table4, path = "tables/Table4.docx")


# Table 5: Sources of information    
table5 <- ds %>% 
  dplyr::select(41:49) %>% 
  tbl_summary(missing = "no") %>% 
  as_flex_table() %>% 
  set_table_properties(width = 1, layout = "autofit")

table5 
# Export table 5
save_as_docx(table5, path = "tables/Table5.docx")


# Table 6: Reasons for self-medication   
table6 <- ds %>% 
  dplyr::select(51:62) %>% 
  mutate(Others = `Others...62`) %>% 
  tbl_summary(missing = "no") %>% 
  as_flex_table() %>% 
  set_table_properties(width = 1, layout = "autofit")

table6 
# Export table 6
save_as_docx(table6, path = "tables/Table6.docx")


