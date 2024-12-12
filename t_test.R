library(tidyverse)
library(infer)

t_tests <- read_csv("~/R Stuff/nationalityBias/t_tests.csv")

combined <- read_csv("~/R Stuff/nationalityBias/combined_for_residual_analysis.csv")

t_tests <- t_tests %>% 
  select(groups:average)

resids <- combined %>% 
  select(nationality_group, resid) %>% 
  mutate(nationality_group = ifelse(nationality_group == "England", "England", "World"))

two_groups <- c("England", "World")
two_group_resids <- resids %>% 
  filter(nationality_group %in% two_groups) %>% 
  mutate(nationality_group = factor(nationality_group))


p_value_traditional <- two_group_resids %>% 
  t_test(
    formula = resid ~ nationality_group,
    order = two_groups,
    alternative = "two-sided"
  )

observed_statistic <- two_group_resids %>%
  specify(response = resid, explanatory = nationality_group) %>%
  calculate(stat = "diff in medians", order = two_groups)

null_dist_simulated <- two_group_resids %>%
  specify(response = resid, explanatory = nationality_group) %>%
  hypothesize(null = "independence") %>%
  generate(reps = 1000, type = "permute") %>%
  calculate(stat = "diff in means", order = two_groups)

null_dist_simulated %>%
  visualize() +
  shade_p_value(observed_statistic, direction = "two-sided")

p_value_simulated <- null_dist_simulated %>%
  get_p_value(
    obs_stat = observed_statistic,
    direction = "two-sided"
  )

p_value_traditional
p_value_simulated

new_t_test <- data.frame(groups = paste(two_groups[1],two_groups[2], sep = ", "),
                         traditional = p_value_traditional$p_value[1],
                         simulated = 0) %>% 
  mutate(average = (simulated + traditional)/2)

t_tests <- rbind(t_tests, new_t_test) %>% 
  arrange(average) %>% 
  distinct(groups, .keep_all = TRUE) 


t_tests$traditional <-  format(t_tests$traditional, scientific = FALSE)

#t_tests <- t_tests[-c(1), ]

write.csv(t_tests, "~/R Stuff/nationalityBias/t_tests.csv")

# 
# t_tests <- data.frame(groups = "test",
#                          traditional = 0,
#                          simulated = 0) %>%
#   mutate(average = (simulated + traditional)/2)

ggplot(data = resids, mapping = aes(x = reorder(nationality_group, resid, FUN = median), y = resid)) +
  geom_boxplot()
