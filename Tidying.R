library("tidyverse")
library("worldfootballR")
library("janitor")
library("lubridate")



players_22 <- read_csv("~/R Stuff/players_22.csv")
players_21 <- read_csv("~/R Stuff/players_21.csv")
players_20 <- read_csv("~/R Stuff/players_20.csv")
players_19 <- read_csv("~/R Stuff/players_19.csv")
players_18 <- read_csv("~/R Stuff/players_18.csv")
players_17 <- read_csv("~/R Stuff/players_17.csv")
nationality_groups <- read.csv("~/R Stuff/nationality_groups.csv")
squad_position <- read.csv("~/R Stuff/squad_position.csv")
player_names <- read.csv("~/R Stuff/player_names.csv")
club_joined <- read.csv("~/R Stuff/club_joined.csv")


players_22 %>% 
  count(sofifa_id) %>% 
  filter(n > 1)

players_21 %>% 
  count(sofifa_id) %>% 
  filter(n > 1)

players_20 %>% 
  count(sofifa_id) %>% 
  filter(n > 1)

players_19 %>% 
  count(sofifa_id) %>% 
  filter(n > 1)

players_18 %>% 
  count(sofifa_id) %>% 
  filter(n > 1)

players_17 %>% 
  count(sofifa_id) %>% 
  filter(n > 1)

players_shooting_22 <- select(subset(players_22, player_positions !="GK"), 
                               sofifa_id, short_name:player_positions, 
                               club_name,
                               league_name,
                               nationality_name,
                               club_joined, 
                               overall, 
                               shooting, 
                               attacking_finishing, 
                               mentality_positioning, 
                               power_shot_power, 
                               power_long_shots, 
                               mentality_penalties, 
                               attacking_volleys)

players_shooting_21 <- select(subset(players_21, player_positions !="GK"), 
                           sofifa_id, short_name:player_positions, 
                           club_name,
                           league_name,
                           nationality_name,
                           club_joined,
                           overall, 
                           shooting, 
                           attacking_finishing, 
                           mentality_positioning, 
                           power_shot_power, 
                           power_long_shots, 
                           mentality_penalties, 
                           attacking_volleys)

players_shooting_20 <- select(subset(players_20, player_positions !="GK"), 
                           sofifa_id, short_name:player_positions, 
                           club_name,
                           league_name,
                           nationality_name,
                           club_joined,
                           overall, 
                           shooting, 
                           attacking_finishing, 
                           mentality_positioning, 
                           power_shot_power, 
                           power_long_shots, 
                           mentality_penalties, 
                           attacking_volleys)

players_shooting_19 <- select(subset(players_19, player_positions !="GK"), 
                           sofifa_id, short_name:player_positions, 
                           club_name,
                           league_name,
                           nationality_name,
                           club_joined,
                           overall, 
                           shooting, 
                           attacking_finishing, 
                           mentality_positioning, 
                           power_shot_power, 
                           power_long_shots, 
                           mentality_penalties, 
                           attacking_volleys)

players_shooting_18 <- select(subset(players_18, player_positions !="GK"), 
                           sofifa_id, short_name:player_positions, 
                           club_name,
                           league_name,
                           nationality_name,
                           club_joined,
                           overall, 
                           shooting, 
                           attacking_finishing, 
                           mentality_positioning, 
                           power_shot_power, 
                           power_long_shots, 
                           mentality_penalties, 
                           attacking_volleys)

players_shooting_17 <- select(subset(players_17, club_position !="GK"), 
                           sofifa_id, short_name:player_positions, 
                           club_name,
                           league_name,
                           nationality_name,
                           club_joined,
                           overall, 
                           shooting, 
                           attacking_finishing, 
                           mentality_positioning, 
                           power_shot_power, 
                           power_long_shots, 
                           mentality_penalties, 
                           attacking_volleys)

players_shooting_22 <-  mutate(players_shooting_22, year = 2022)
players_shooting_21 <-  mutate(players_shooting_21, year = 2021)
players_shooting_20 <-  mutate(players_shooting_20, year = 2020)
players_shooting_19 <-  mutate(players_shooting_19, year = 2019)
players_shooting_18 <-  mutate(players_shooting_18, year = 2018)
players_shooting_17 <-  mutate(players_shooting_17, year = 2017)

players_1 <-  full_join(players_shooting_22, players_shooting_21, by = "sofifa_id", suffix = c("_22", "_21")) %>% 
    full_join(players_shooting_20, by = "sofifa_id", suffix = c("", "")) %>% 
    full_join(players_shooting_19, by = "sofifa_id", suffix = c("_20", "_19")) %>% 
    full_join(players_shooting_18, by = "sofifa_id", suffix = c("", "")) %>% 
    full_join(players_shooting_17, by = "sofifa_id", suffix = c("_18", "_17")) %>% 
    full_join(nationality_groups, by = c("nationality_name_22" = "Nationality")) %>% 
    rename(nationality_group = Group)

player_short_names <- select(players_1, sofifa_id, short_name_22, short_name_21, short_name_20, short_name_19, short_name_18, short_name_17)
player_nationalities <- select(players_1, sofifa_id, short_name_22, nationality_name_22, nationality_name_21, nationality_name_20, nationality_name_19, nationality_name_18, nationality_name_17)

players_2 <- bind_rows(players_shooting_22, players_shooting_21, players_shooting_20, players_shooting_19, players_shooting_18, players_shooting_17)
players_2 <- players_2[, c(1:3, 17, 4:16)] %>% 
  full_join(nationality_groups, by = c("nationality_name" = "Nationality")) %>% 
  rename(nationality_group = Group) %>% 
  separate(short_name, into = c("first", "last"), sep = "^\\S*\\K\\s+") %>% 
  separate(long_name, into = c("first2", "rest"), sep = "^\\S*\\K\\s+") %>%
  mutate(last = coalesce(last, first))

players_2$full_name = paste(ifelse(is.na(players_2$first2), "", players_2$first2), ifelse(is.na(players_2$last), "", players_2$last))

players_2 <- players_2 %>% 
    select(sofifa_id, full_name, first, first2, last, rest, year:nationality_group)%>% 
  mutate(year = year - 1) %>% 
  filter(year > 2017)


player_shooting_stats <- load_fb_big5_advanced_season_stats(
  season_end_year = 2018:2021,
  stat_type = "shooting",
  team_or_player = "player"
) %>%
  as_tibble() %>%
filter(Comp == "Premier League") %>%
  clean_names() %>%
  separate(player, into = c("first", "last_name"), sep = " (?=[^ ]+$)") %>%
  separate(first, into = c("f", "f2"), sep = " (?=[^ ]+$)") %>%
  filter(pos != "GK") %>%
  mutate(first_initial = paste(substr(f, 1, 1), ".", sep = ""))

player_shooting_stats$full_name <- paste(ifelse(is.na(player_shooting_stats$f), "", player_shooting_stats$f),
                                         ifelse(is.na(player_shooting_stats$f2), "", player_shooting_stats$f2),
                                         ifelse(is.na(player_shooting_stats$last_name), "", player_shooting_stats$last_name)) %>%
  str_replace_all("  "," ")


combined_names <- left_join(player_shooting_stats, players_2, by = c("full_name", "season_end_year" = "year")) %>%
  filter(league_name == "English Premier League" | is.na(league_name)) %>%
  filter(mins_per_90 > 0)

combined_names <- left_join(combined_names, players_2, by = c("season_end_year" = "year", "first_initial" = "first", "last_name" = "last"), suffix(".x", ".y"))
combined_names <- left_join(combined_names, players_2, by = c("season_end_year" = "year", "f" = "first", "last_name" = "last"), suffix(".z", ".a"))
players_2 <- players_2 %>% 
  mutate(l = last)

combined_names <- combined_names %>% 
  mutate(
    last_name = coalesce(last_name, f)
  )
combined_names <- left_join(combined_names, players_2, by = c("season_end_year" = "year", "last_name" = "l")) %>% 
  filter(league_name.x == "English Premier League" | is.na(league_name.x)) %>%
  filter(league_name.y == "English Premier League" | is.na(league_name.y)) %>%
  filter(league_name.x.x == "English Premier League" | is.na(league_name.x.x)) %>%
  filter(league_name.y.y == "English Premier League" | is.na(league_name.y.y)) 




players_2 <- players_2 %>% 
  mutate(long_name = paste(first2, rest))


combined_names <- combined_names %>%
  mutate(
    full_name = coalesce(full_name.x, full_name.y, full_name.x.x, full_name.y.y),
    sofifa_id = coalesce(sofifa_id.x, sofifa_id.y, sofifa_id.x.x, sofifa_id.y.y),
    player_positions = coalesce(player_positions.x, player_positions.y, player_positions.x.x, player_positions.y.y),
    club_name = coalesce(club_name.x, club_name.y, club_name.x.x, club_name.y.y),
    league_name = coalesce(league_name.x, league_name.y, league_name.x.x, league_name.y.y),
    nationality_name = coalesce(nationality_name.x, nationality_name.y, nationality_name.x.x, nationality_name.y.y),
    overall = coalesce(overall.x, overall.y, overall.x.x, overall.y.y),
    shooting = coalesce(shooting.x, shooting.y, shooting.x.x, shooting.y.y),
    attacking_finishing = coalesce(attacking_finishing.x, attacking_finishing.y, attacking_finishing.x.x, attacking_finishing.y.y),
    mentality_positioning = coalesce(mentality_positioning.x, mentality_positioning.y, mentality_positioning.x.x, mentality_positioning.y.y),
    power_shot_power = coalesce(power_shot_power.x, power_shot_power.y, power_shot_power.x.x, power_shot_power.y.y),
    power_long_shots = coalesce(power_long_shots.x, power_long_shots.y, power_long_shots.x.x, power_long_shots.y.y),
    mentality_penalties = coalesce(mentality_penalties.x, mentality_penalties.y, mentality_penalties.x.x, mentality_penalties.y.y),
    attacking_volleys = coalesce(attacking_volleys.x, attacking_volleys.y, attacking_volleys.x.x, attacking_volleys.y.y),
    nationality_group = coalesce(nationality_group.x, nationality_group.y, nationality_group.x.x, nationality_group.y.y),
    club_joined = coalesce(club_joined.x, club_joined.x.x, club_joined.y, club_joined.y.y)
  ) %>%
  filter(league_name == "English Premier League" | is.na(league_name))


combined_names <- left_join(combined_names, player_names, by = "full_name", "season_end_year")
combined_names <- left_join(combined_names, players_2, by = c("long_name", "season_end_year.x" = "year"))

combined_names <- combined_names %>%
  mutate(
    full_name = coalesce(full_name.x, full_name.y, full_name.x.x, full_name.y.y, full_name.x.x.x, full_name.y.y.y),
    sofifa_id = coalesce(sofifa_id.x, sofifa_id.y, sofifa_id.x.x, sofifa_id.y.y, sofifa_id.x.x.x, sofifa_id.y.y.y),
    player_positions = coalesce(player_positions.x, player_positions.y, player_positions.x.x, player_positions.y.y, player_positions.x.x.x, player_positions.y.y.y),
    club_name = coalesce(club_name.x, club_name.y, club_name.x.x, club_name.y.y, club_name.x.x.x, club_name.y.y.y),
    league_name = coalesce(league_name.x, league_name.y, league_name.x.x, league_name.y.y, league_name.x.x.x, league_name.y.y.y),
    nationality_name = coalesce(nationality_name.x, nationality_name.y, nationality_name.x.x, nationality_name.y.y, nationality_name.x.x.x, nationality_name.y.y.y),
    overall = coalesce(overall.x, overall.y, overall.x.x, overall.y.y, overall.x.x.x, overall.y.y.y),
    shooting = coalesce(shooting.x, shooting.y, shooting.x.x, shooting.y.y, shooting.x.x.x, shooting.y.y.y),
    attacking_finishing = coalesce(attacking_finishing.x, attacking_finishing.y, attacking_finishing.x.x, attacking_finishing.y.y, attacking_finishing.x.x.x, attacking_finishing.y.y.y),
    mentality_positioning = coalesce(mentality_positioning.x, mentality_positioning.y, mentality_positioning.x.x, mentality_positioning.y.y, mentality_positioning.x.x.x, mentality_positioning.y.y.y),
    power_shot_power = coalesce(power_shot_power.x, power_shot_power.y, power_shot_power.x.x, power_shot_power.y.y, power_shot_power.x.x.x, power_shot_power.y.y.y),
    power_long_shots = coalesce(power_long_shots.x, power_long_shots.y, power_long_shots.x.x, power_long_shots.y.y, power_long_shots.x.x.x, power_long_shots.y.y.y),
    mentality_penalties = coalesce(mentality_penalties.x, mentality_penalties.y, mentality_penalties.x.x, mentality_penalties.y.y, mentality_penalties.x.x.x, mentality_penalties.y.y.y),
    attacking_volleys = coalesce(attacking_volleys.x, attacking_volleys.y, attacking_volleys.x.x, attacking_volleys.y.y, attacking_volleys.x.x.x, attacking_volleys.y.y.y),
    nationality_group = coalesce(nationality_group.x, nationality_group.y, nationality_group.x.x, nationality_group.y.y, nationality_group.x.x.x, nationality_group.y.y.y),
    club_joined = coalesce(club_joined.x, club_joined.x.x, club_joined.y, club_joined.y.y, club_joined.x.x.x, club_joined.y.y.y)
  ) %>%
  filter(league_name == "English Premier League" | is.na(league_name)) %>% 
  filter(!is.na(overall))

combined <- combined_names %>% 
  select(season_end_year.x, squad, nation:url, full_name:nationality_group, club_joined) %>% 
  mutate(season_start = season_end_year.x - 1,
         season_end = season_end_year.x,
         season = paste(season_start, season_end, sep = "-")) %>% 
  select(full_name, season, squad:nationality_group, club_joined)



combined <- combined[!duplicated(combined), ]

players_2 <- players_2 %>%
  arrange(full_name)

player_shooting_stats <- player_shooting_stats %>%
  arrange(full_name)

squad_position[6] <- NA

colnames(squad_position) <- c("squad", "2017-2018", "2018-2019", "2019-2020", "2020-2021", "squad_group")

squad_position <- squad_position %>% 
  pivot_longer(c("2017-2018", "2018-2019", "2019-2020", "2020-2021"), names_to = "season", values_to = "table_position") %>% 
  filter(!is.na(table_position))  

squad_position <- squad_position %>% 
  mutate(
    squad_group = ifelse(table_position <= 4, "Top4", ifelse(table_position <= 7, "Europa", ifelse(table_position <= 17, "MidTable", "Relegation")))
  )

combined <- combined %>% 
  full_join(squad_position, by = c("squad", "season")) %>% 
  mutate(sh_per_game = sh_standard / mins_per_90)

names(combined)[names(combined) == 'mins_per_90'] <- 'games'
names(combined)[names(combined) == 'gls_standard'] <- 'goals'
names(combined)[names(combined) == 'sh_standard'] <- 'shots'
names(combined)[names(combined) == 'so_t_standard'] <- 'sot'
names(combined)[names(combined) == 'gls_standard'] <- 'goals'
names(combined)[names(combined) == 'gls_standard'] <- 'goals'

names(combined)[names(combined) == 'attacking_finishing'] <- 'finishing'
names(combined)[names(combined) == 'mentality_positioning'] <- 'positioning'
names(combined)[names(combined) == 'power_shot_power'] <- 'shot_power'
names(combined)[names(combined) == 'power_long_shots'] <- 'long_shots'
names(combined)[names(combined) == 'mentality_penalties'] <- 'penalties'
names(combined)[names(combined) == 'attacking_volleys'] <- 'volleys'

combined <- combined %>% 
  drop_na(full_name)

combined <- combined %>% left_join(club_joined, by = c("full_name", "season", "squad")) %>% 
  mutate(club_joined = coalesce(club_joined.x, as_date(club_joined.y))) %>% 
  select(full_name:nationality_group, squad_group:sh_per_game, club_joined) %>% 
  mutate(season_end = substring(season, 6, 9),
         season_end = as.Date(ISOdate(season_end, 10, 1)),
         days_at_club = as.integer(season_end - club_joined)) %>% 
  select(full_name:club_joined, days_at_club)

na_players <- combined %>% 
  filter(is.na(club_joined)) %>% 
  select(season, full_name, squad)

combined <- combined %>% 
  separate(player_positions, c("primary_position", "secondary_positions"), ",", extra = "merge")

write.csv(combined, "combined.csv")
write.csv(na_players, "na_players.csv")
