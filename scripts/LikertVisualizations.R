library(tidyverse)
library(likert)
library(ggpubr)
library(ggthemes)
library(RColorBrewer)

ds <- readxl::read_excel("data/AMR_KAP_No_Code.xlsx")

# Visualize knowledge response 
knowledge_df <- ds %>% 
  select(12:23) %>% 
  mutate_if(is.character, as.factor) %>% 
  as.data.frame() 

# Plot in likert scale 
p1 <- plot(likert(knowledge_df), 
     ordered = FALSE, 
     group.order = names(knowledge_df), 
     center = 2) 
p1 + theme_pubr()

ggsave("figures/Knowledge.tiff", units="in", width=12, height=6, dpi=600, compression = 'lzw')


# Visualize attitude response 
attitude_df <- ds %>% 
  select(24:33) %>% 
  mutate_if(is.character, as.factor) %>% 
  as.data.frame() 


# Plot in likert scale 
p2 <- plot(likert(attitude_df), 
           ordered = FALSE, 
           group.order = names(attitude_df), 
           center = 2) 
p2 + theme_pubr()

ggsave("figures/attitude.tiff", units="in", width=12, height=6, dpi=600, compression = 'lzw')


# Visualize practices response 
practices_df <- ds %>% 
  dplyr::select(34:39) %>% 
  mutate_if(is.character, as.factor) %>% 
  as.data.frame() 

# Plot in likert scale 
p3 <- plot(likert(practices_df), ordered = F, wrap= 60)
p3+ theme_pubr()
ggsave("figures/practices.tiff", units="in", width=12, height=6, dpi=600, compression = 'lzw')

