library(tidyverse)
library(rlang)

library(modelr)
options(na.action = na.warn)

combined <- read_csv("~/R Stuff/nationalityBias/combined.csv") 
adj_r_squared <- read_csv("~/R Stuff/nationalityBias/adj_r_squared.csv")
adj_r_squared <- adj_r_squared %>% 
  select(intercept, r_squared, filter, terms, sh_filter, sot_filter, players)

#np_g_minus_x_g_expected
#g_per_sh_standard

#adj_r_squared <- adj_r_squared[-c(80), ]
#adj_r_squared$filter <- c("none")



filter_var <- "goals > 0 & goals != pk_standard"

if(filter_var == "none"){
  
}else{
  combined <- combined %>% 
    filter(eval(parse(text={{filter_var}})))
}

filter_var_shots <- "shots > 0"

if(filter_var_shots == "none"){
  
}else{
  combined <- combined %>% 
    filter(eval(parse(text={{filter_var_shots}})))
  
}

filter_var_sot <- "sot > 0"

if(filter_var_sot == "none"){
  
}else{
  combined <- combined %>% 
    filter(eval(parse(text={{filter_var_sot}})))
  
}

# combined <- combined %>% 
#   filter(pos == "DF")

#mod1 <- lm(shooting ~ log(goals - pk_standard) + primary_position + squad + dist_standard + sh_per_game + age + season, data = combined)

mod1 <- lm(shooting ~ log(goals - pk_standard) + primary_position + squad + dist_standard + sh_per_game + age + season, data = combined)
summary(mod1)

new_adj_r_squared <- data.frame(intercept = toString(attr(mod1$terms , "term.labels")),
              r_squared = summary(mod1)$adj.r.squared) %>% 
  mutate(
              filter = {{filter_var}},
              terms = str_count(intercept, ",") + 1,
              sh_filter = {{filter_var_shots}},
              sot_filter = {{filter_var_sot}},
              players = nrow(combined)
              )


adj_r_squared <- rbind(adj_r_squared, new_adj_r_squared) %>% 
  arrange(players) %>% 
  arrange(desc(r_squared))

combined <- combined %>% 
  add_predictions(mod1) %>% 
  add_residuals(mod1)

ggplot(combined, aes(shooting, resid), position = "jitter") +
  geom_ref_line(h = 0) +
  geom_jitter(aes(color = g_minus_x_g_expected))

i <- 2

while(i < nrow(adj_r_squared)){
  if(!is.na(adj_r_squared$players[i])){
      if(near(adj_r_squared$r_squared[i], adj_r_squared$r_squared[i - 1]) & adj_r_squared$players[i] == adj_r_squared$players[i - 1]){
        adj_r_squared <- adj_r_squared[-c(i), ]
          i <- i - 1
      }
  }
  i <- i + 1
}

adj_r_squared <- adj_r_squared %>% 
  mutate(terms = str_count(intercept, ",") + 1)

summary <- summary(mod1)
summary(mod1)$adj.r.squared

write.csv(adj_r_squared, "adj_r_squared.csv")
write.csv(combined, "combined_for_residual_analysis.csv")
