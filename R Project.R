library("tidyverse")

players_22 <- read_csv("~/R Stuff/players_22.csv")
players_21 <- read_csv("~/R Stuff/players_21.csv")
players_20 <- read_csv("~/R Stuff/players_20.csv")
players_19 <- read_csv("~/R Stuff/players_19.csv")
players_18 <- read_csv("~/R Stuff/players_18.csv")
players_17 <- read_csv("~/R Stuff/players_17.csv")

prem_22 <- filter(players_22, league_name == "English Premier League")
prem_21 <- filter(players_21, league_name == "English Premier League")
prem_20 <- filter(players_20, league_name == "English Premier League")
prem_19 <- filter(players_19, league_name == "English Premier League")
prem_18 <- filter(players_18, league_name == "English Premier League")
prem_17 <- filter(players_17, league_name == "English Premier League")

prem_avg_shooting_22 <- select(subset(prem_22, club_position !="GK"), 
                               sofifa_id:short_name, 
                               nationality_name,
                               overall, 
                               shooting, 
                               attacking_finishing, 
                               mentality_positioning, 
                               power_shot_power, 
                               power_long_shots, 
                               mentality_penalties, 
                               attacking_volleys)
prem_avg_shooting_22 <- mutate(prem_avg_shooting_22, 
       avg = (attacking_finishing +
       mentality_positioning + 
       power_shot_power + 
       power_long_shots + 
       mentality_penalties + 
       attacking_volleys)/6
       )

prem_avg_shooting_22 <- prem_avg_shooting_22[, c(1:5, 12, 6:11)]

ggplot(data = prem_avg_shooting_22) +
  geom_point(mapping = aes(x = shooting, y = avg), position = "jitter")


