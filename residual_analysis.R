library(tidyverse)
library(rlang)

library(modelr)
options(na.action = na.warn)

model_combined <- read_csv("~/R Stuff/nationalityBias/combined_for_residual_analysis.csv") 

combined <- model_combined %>% 
  select(full_name:squad, nationality_name, nationality_group, resid, shooting, pred, pos, overall, games, squad_group, goals, fk_standard, shots) %>% 
  mutate(percent_error = resid / shooting)

model <- lm(shooting ~ pred, data = combined)

# by_nation <- combined %>% group_by(nationality_name) %>% 
#   summarize(mean = mean(resid),
#             sd = sd(resid),
#             first_quartile = quantile(resid, 0.25),
#             median = median(resid),
#             third_quartile = quantile(resid, 0.75),
#             count = n())
# 
# by_nation_percent_error <- combined %>% group_by(nationality_name) %>% 
#   summarize(mean = mean(percent_error),
#             sd = sd(percent_error),
#             first_quartile = quantile(percent_error, 0.25),
#             median = median(percent_error),
#             third_quartile = quantile(percent_error, 0.75),
#             count = n())
# 
by_group <- combined %>% group_by(nationality_group) %>%
  summarize(mean = mean(resid),
            sd = sd(resid),
            first_quartile = quantile(resid, 0.25),
            median = median(resid),
            third_quartile = quantile(resid, 0.75),
            count = n())
# 
# by_group_percent_error <- combined %>% group_by(nationality_group) %>% 
#   summarize(mean = mean(percent_error),
#             sd = sd(percent_error),
#             first_quartile = quantile(percent_error, 0.25),
#             median = median(percent_error),
#             third_quartile = quantile(percent_error, 0.75),
#             count = n())
# 
# by_pos <- combined %>% group_by(pos) %>% 
#   summarize(mean = mean(resid),
#             sd = sd(resid),
#             first_quartile = quantile(resid, 0.25),
#             median = median(resid),
#             third_quartile = quantile(resid, 0.75),
#             count = n())
# 
# by_overall <- combined %>% group_by(overall) %>% 
#   summarize(mean = mean(resid),
#             sd = sd(resid),
#             first_quartile = quantile(resid, 0.25),
#             median = median(resid),
#             third_quartile = quantile(resid, 0.75),
#             count = n())
# 
# above_85 <- combined %>% 
#   filter(overall > 84)
# 
# above_80 <- combined %>% 
#   filter(overall > 79)
# 
# above_85_by_group <- above_85 %>% group_by(nationality_group) %>% 
#   summarize(mean = mean(resid),
#             sd = sd(resid),
#             first_quartile = quantile(resid, 0.25),
#             median = median(resid),
#             third_quartile = quantile(resid, 0.75),
#             count = n())
# 
# above_80_by_group <- above_80 %>% group_by(nationality_group) %>% 
#   summarize(mean = mean(resid),
#             sd = sd(resid),
#             first_quartile = quantile(resid, 0.25),
#             median = median(resid),
#             third_quartile = quantile(resid, 0.75),
#             count = n())
# 
# 
# England <- combined %>% 
#   filter(nationality_name == "England")
# 
# FootballingNations <- combined %>% 
#   filter(nationality_group == "FootballingNations")
# 
# Asia <- combined %>% 
#   filter(nationality_group == "Asia")
# 
# ggplot(data = combined) +
#   geom_histogram(mapping = aes(x = resid))
# 
# ggplot(data = England) +
#   geom_histogram(mapping = aes(x = resid))
# 
# ggplot(data = FootballingNations) +
#   geom_histogram(mapping = aes(x = resid))
# 
# ggplot(data = combined, mapping = aes(x = shooting, y = pred)) +
#   geom_point(mapping = aes(color = pos), position = "jitter") +
#   geom_abline(mapping = NULL, coef(model)[1], coef(model)[2])
# 
# ggplot(data = combined, mapping = aes(x = resid, colour = nationality_group))+
#   geom_freqpoly(binwidth = 1)
# 
ggplot(data = combined, mapping = aes(x = reorder(nationality_group, resid, FUN = median), y = resid)) +
  geom_boxplot()
# 
# ggplot(data = combined, mapping = aes(x = overall, colour = nationality_group))+
#   geom_freqpoly(binwidth = 1)
# 
# ggplot(data = combined, mapping = aes(x = reorder(nationality_group, overall, FUN = median), y = overall)) +
#   geom_boxplot()
# 
# ggplot(data = combined, mapping = aes(x = overall, y = resid)) +
#   geom_boxplot(mapping = aes(group = cut_width(overall, 1)))
# 
# ggplot(data = combined, mapping = aes(x = reorder(pos, resid, FUN = median), y = resid)) +
#   geom_boxplot()
# 
# ggplot(data = combined) + 
#   geom_bar(mapping = aes(x = nationality_group, fill = pos), position = "fill")
# 
# ggplot(data = combined) + 
#   geom_bar(mapping = aes(x = nationality_group, fill = squad_group), position = "fill")
# 
# ggplot(data = combined) + 
#   geom_bar(mapping = aes(x = overall, fill = nationality_group), position = "fill")

ggplot(combined, aes(shooting, resid), position = "jitter") +
  geom_ref_line(h = 0) +
  geom_jitter(aes(color = pos, size = shots))

              