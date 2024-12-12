library(tidyverse)
library(rlang)

library(modelr)
options(na.action = na.warn)

combined <- read_csv("~/R Stuff/nationalityBias/combined.csv") 


NationalityGroup_counts <- combined %>% 
  group_by(nationality_group) %>% 
  summarize(nationality_count = n())

Nationality_rating_counts <- combined %>% 
  group_by(nationality_group, shooting) %>% 
  summarize(count = n()) %>% 
  left_join(NationalityGroup_counts, by = c("nationality_group")) %>% 
  mutate(percent = count/nationality_count) %>% 
  filter(!is.na(nationality_group)) %>% 
  filter(nationality_group != "Asia" & nationality_group != "Other")

ggplot(data = Nationality_rating_counts, mapping = aes(x = shooting, y = percent)) +
  geom_point(mapping = aes(color = nationality_group), position = "jitter") +
  geom_line(mapping = aes(color = nationality_group))



#graph of shooting vs overall