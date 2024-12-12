library("tidyverse")
library("lubridate")
library("incidence")
library("stringr")
library("janitor")
library(rlang)


Africa_England <- read_csv("~/R Stuff/nationalityBias/resampled_t_tests/Africa_England.csv") %>% 
  mutate(comparison = "Africa")
Asia_England <- read_csv("~/R Stuff/nationalityBias/resampled_t_tests/Asia_England.csv") %>% 
  mutate(comparison = "Asia")
FootballingNations_England <- read_csv("~/R Stuff/nationalityBias/resampled_t_tests/FootballingNations_England.csv") %>% 
  mutate(comparison = "FootballingNations")
OtherAmericas_England <- read_csv("~/R Stuff/nationalityBias/resampled_t_tests/OtherAmericas_England.csv") %>% 
  mutate(comparison = "OtherAmericas")
Other_England <- read_csv("~/R Stuff/nationalityBias/resampled_t_tests/Other_England.csv") %>% 
  mutate(comparison = "Other")
OtherEurope_England <- read_csv("~/R Stuff/nationalityBias/resampled_t_tests/OtherEurope_England.csv") %>% 
  mutate(comparison = "OtherEurope")
Tier2_England <- read_csv("~/R Stuff/nationalityBias/resampled_t_tests/Tier2_England.csv") %>% 
  mutate(comparison = "Tier2")
UK_England <- read_csv("~/R Stuff/nationalityBias/resampled_t_tests/UK_England.csv") %>% 
  mutate(comparison = "UK")
Rest_England <- read_csv("~/R Stuff/nationalityBias/resampled_t_tests/Rest_England.csv") %>% 
  mutate(comparison = "Rest",
         estimate = estimate*(-1))
combined <- read_csv("~/R Stuff/nationalityBias/combined.csv") %>% 
  select(-1)

#Africa_FootballingNations <- read_csv("~/R Stuff/nationalityBias/resampled_t_tests/Africa_FootballingNations.csv") %>% 
 # mutate(comparison = "Afr_foot")

Nationality_counts <- combined %>% 
  group_by(nationality_name) %>% 
  summarize(count = n())

Nationality_group_counts <- combined %>% 
  group_by(nationality_group) %>% 
  summarize(count = n())

quantile(Rest_England$p_value, 0.25)
quantile(Rest_England$p_value, 0.75)
quantile(Rest_England$p_value, 0.90)

quantile(FootballingNations_England$p_value, 0.25)
quantile(FootballingNations_England$p_value, 0.75)
quantile(FootballingNations_England$p_value, 0.90)


combined_results <- rbind(Africa_England, Asia_England, FootballingNations_England, OtherAmericas_England, Other_England, OtherEurope_England, Tier2_England, UK_England, Rest_England)


ggplot(data = combined_results, mapping = aes(x = reorder(comparison, estimate, FUN = median), y = estimate)) +
  geom_boxplot() +
  ggtitle("Boxplots of Differences from England") +
  labs(x = "Nationality Group", y = "Estimate (Nationality Group - England)") + 
  geom_hline(yintercept = 0)
  

ggplot(data = combined_results, mapping = aes(x = reorder(comparison, estimate, FUN = median), y = p_value)) +
  geom_boxplot() + 
  ggtitle("Boxplots of P Values by Nationality Group Compared with England") +
  labs(x = "Nationality Group", y = "P Values")+ 
  geom_hline(yintercept = 0.05, color = "red")


FootballingNations_England$Significant <- FootballingNations_England$p_value <= 0.05
Rest_England$Significant <- Rest_England$p_value <= 0.05


table(FootballingNations_England$Significant)
mean(FootballingNations_England$p_value)

table(Rest_England$Significant)
mean(Rest_England$p_value)

FootballingNations_England |> 
  ggplot() +
  aes(x = p_value, fill = Significant) +
  ylim(0, 500) +
  xlim(NA, 0.5) +
  geom_histogram(binwidth = 0.005, boundary = 0) + 
  ggtitle("P Values of FootballingNations vs England")

Rest_England |> 
  ggplot() +
  aes(x = p_value, fill = Significant) +
  ylim(0, 500) +
  xlim(NA, 0.5) +
  geom_histogram(binwidth = 0.005, boundary = 0) + 
  ggtitle("P Values of Rest of World vs England")









