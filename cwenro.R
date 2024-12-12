library("ggplot2")
library("tidyverse")
library("lubridate")
library("incidence")
library("stringr")
library("janitor")
library("readr")
library("dplyr")
library("modelr")
library("leaps")
library("ggrepel")
library("ggplot2")
library("broom")
library("patchwork")
library("GGally")
library("tidyverse")
library("ggrepel")
library("MASS")

#Download
d_df <- read_csv("~/R Stuff/fpl/data.csv") %>% 
  filter(Points > 0) 
d_mp <- read_csv("~/R Stuff/fpl/minutes_played.csv") 

#Names
d_df$Player <- str_replace_all(d_df$Player, "[^[:alnum:]]", " ")
d_df$Player <- str_replace_all(d_df$Player, "\\s+", " ")
d_df$Player <- trimws(d_df$Player)
d_df <- d_df %>% 
  rename("Squad" = "Team")

d_mp$Player <- str_replace_all(d_mp$Player, "[^[:alnum:]]", " ")
d_mp$Player <- str_replace_all(d_mp$Player, "\\s+", " ")
d_mp$Player <- trimws(d_mp$Player)

d_mp[d_mp == "Manchester City"] <- "Man City"
d_mp[d_mp == "Manchester Utd"] <- "Man Utd"
d_mp[d_mp == "Newcastle Utd"] <- "Newcastle"
d_mp[d_mp == "Tottenham"] <- "Spurs"
d_mp[d_mp == "Nott'ham Forest"] <- "Nott'm Forest"


d_df <- d_df %>% 
  mutate(full_name = Player) %>% 
  separate(Player, into = c("first", "last"), sep = "^\\S*\\K\\s+")

d_mp <- d_mp %>% 
  mutate(full_name = Player) %>% 
  separate(Player, into = c("first", "last"), sep = "^\\S*\\K\\s+")

#First Join
combined <- left_join(d_df, d_mp, by = c("full_name", "first", "last", "Squad")) 
mp <- d_mp %>% 
  left_join(combined, by = c("Rk", "full_name", "first", "last", "Squad", "Age", "Born", "MP", "Starts", "Min")) %>% 
  filter(is.na(Points)) %>% 
  dplyr::select(Rk:full_name)
rest <- combined %>% 
  filter(is.na(Min)) %>% 
  dplyr::select(first:full_name)
combined <- combined %>% 
  filter(!is.na(Min)) %>% 
  dplyr::select(full_name, Squad:Team_Quality, Age:Min)

#Second Join
rest <- left_join(rest, mp, by = c("full_name" = "last", "Squad")) %>% 
  mutate(last.2 = full_name)
mp <- mp %>% 
  left_join(rest, by = c("Rk", "full_name" = "full_name.y", "first" = "first.y", "last" = "last.2", "Squad", "Age", "Born", "MP", "Starts", "Min")) %>% 
  filter(is.na(Points)) %>% 
  dplyr::select(Rk:full_name)
t <- rest %>% 
  filter(!is.na(Min)) %>% 
  dplyr::select(full_name, Squad:Team_Quality, Age:Min)
combined <- rbind(combined, t)

rest <- rest %>% 
  filter(is.na(Min)) %>% 
  dplyr::select(first.x:full_name)

#Third Join
rest <- left_join(rest, mp, by = c("full_name" = "first", "Squad")) %>% 
  mutate(first.2 = full_name)
mp <- mp %>% 
  left_join(rest, by = c("Rk", "full_name" = "full_name.y", "first" = "first.2", "last" = "last.y", "Squad", "Age", "Born", "MP", "Starts", "Min")) %>% 
  filter(is.na(Points)) %>% 
  dplyr::select(Rk:full_name)
t <- rest %>% 
  filter(!is.na(Min)) %>% 
  dplyr::select(full_name, Squad:Team_Quality, Age:Min)
combined <- rbind(combined, t)

rest <- rest %>% 
  filter(is.na(Min)) %>% 
  dplyr::select(first.x:full_name)

#Fourth Join
rest <- rest %>%
  mutate(first_initial = paste(substr(first.x, 1, 1), "", sep = ""))
mp <- mp %>%
  mutate(first_initial = paste(substr(first, 1, 1), "", sep = ""))

rest <- left_join(rest, mp, by = c("first_initial", "last.x" = "last", "Squad"))
mp <- mp %>% 
  left_join(rest, by = c("Rk", "full_name" = "full_name.y", "first", "last" = "last.x", "Squad", "Age", "Born", "MP", "Starts", "Min")) %>% 
  filter(is.na(Points)) %>% 
  dplyr::select(Rk:full_name)

t <- rest %>% 
  filter(!is.na(Min)) %>% 
  dplyr::select(full_name.x, Squad:Team_Quality, Age:Min) %>% 
  rename("full_name" = "full_name.x")
combined <- rbind(combined, t)

rest <- rest %>% 
  filter(is.na(Min)) %>% 
  dplyr::select(first.x:full_name.x)

####
na_vals <- combined %>% 
  filter(is.na(Min))

na_vals2 <- combined %>% 
  filter(is.na(Points))

combined <- combined %>%
  mutate(age_squared = Age^2)

#

mod <- lm(Points ~ (Team_Quality + Position + log(Cost) + Age + age_squared + Min), combined)
summary(mod)

combined$pred <-predict(mod, newdata = combined) 
