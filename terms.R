library(tidyverse)
library(rlang)

library(modelr)
options(na.action = na.warn)

adj_r_squared <- read_csv("~/R Stuff/nationalityBias/adj_r_squared.csv")
adj_r_squared <- adj_r_squared %>% 
  select(intercept, r_squared, filter) %>% 
  mutate(terms = str_count(intercept, ",") + 1) %>% 
  mutate(sh_filter = "shots > 5") %>% 
  mutate(sot_filter = "sot > 0") %>% 
  mutate(players = NA)
  

