library(tidyverse)

tracking_week_1 <- read.csv('tracking_week_1.csv')
tracking_week_2 <- read.csv('tracking_week_2.csv')
tracking_week_3 <- read.csv('tracking_week_3.csv')
tracking_week_4 <- read.csv('tracking_week_4.csv')
tracking_week_5 <- read.csv('tracking_week_5.csv')
tracking_week_6 <- read.csv('tracking_week_6.csv')
tracking_week_7 <- read.csv('tracking_week_7.csv')
tracking_week_8 <- read.csv('tracking_week_8.csv')
tracking_week_9 <- read.csv('tracking_week_9.csv')

tracking_data <- bind_rows(tracking_week_1, tracking_week_2, tracking_week_3,
                           tracking_week_4, tracking_week_5, tracking_week_6,
                           tracking_week_7, tracking_week_8, tracking_week_9)

tackles <- read.csv('tackles.csv')
players <- read.csv('players.csv') |> 
  mutate(defensive_player = case_when(position == "DE" ~ "Y",
                                      position == "NT" ~ "Y",
                                      position == "SS" ~ "Y",
                                      position == "FS" ~ "Y",
                                      position == "OLB" ~ "Y",
                                      position == "DT" ~ "Y",
                                      position == "CB" ~ "Y",
                                      position == "ILB" ~ "Y",
                                      position == "MLB" ~ "Y",
                                      position == "DB" ~ "Y",
                                      TRUE ~ position),
         defensive_player = if_else(defensive_player == "Y", "Y", "N")) #joined

plays <- read.csv('plays.csv') #joined

# Match plays to tracking data by gameId and playId
# Match players to tracking data - 
# Only want tackles attached to tracking data of the player getting tracked
# When building model, only want the defensive players

#df <- plays |>
#  left_join(tackles, by = c("gameId" = "gameId", "playId" = "playId"), na_matches = "never")

pbp <- nflreadr::load_pbp(seasons = 2022) |>
  filter(week <= 8) |>
  dplyr::select(old_game_id, play_id, play_type, run_gap) |>
  mutate(old_game_id = as.integer(old_game_id),
         play_id = as.numeric(play_id))

tracking_data <- tracking_data |>
  left_join(players, by = c("nflId" = "nflId"), na_matches = "never") |>
  rename(displayName = displayName.x) |>
  select(-displayName.y) |>
  left_join(plays, by = c('gameId' = 'gameId', 'playId' = 'playId'), na_matches = "never") |>
  left_join(tackles, by = c('gameId' = 'gameId', 'playId' = 'playId', 'nflId' = 'nflId'), na_matches = 'never', relationship = 'many-to-many') |>
  left_join(pbp, by = c('gameId' = 'old_game_id', 'playId' = 'play_id'), na_matches = "never") 
  

saveRDS(tracking_data, "tracking_data.rds")


tracking_data <- readRDS('tracking_data.rds')

tracking_data_football <- tracking_data |>
  filter(displayName == "football")

saveRDS(tracking_data_football, "tracking_data_football.rds")

tracking_data_offense <- tracking_data |>
  filter(defensive_player == "N")

saveRDS(tracking_data_offense, "tracking_data_offense.rds")

tracking_data_defense <- tracking_data |>
  filter(defensive_player == "Y")


saveRDS(tracking_data_defense, "tracking_data_defense.rds")

## play_dplay_type

# Add Football location to Player Tracking data ---------------------------

tracking_data_football <- readRDS('tracking_data_football.rds')
tracking_data_football <- tracking_data_football |>
  dplyr::select(gameId, playId, displayName, frameId, time,
                x, y, s, a, dis, o, dir) |>
  rename(football_name = displayName, football_time = time,
         football_x = x, football_y = y, football_s = s,
         football_a = a, football_dis = dis, football_o = o,
         football_dir = dir)

tracking_data <- readRDS('tracking_data_defense.rds') #match by, gameId, playId, frameId
tracking_data <- tracking_data |>
  left_join(tracking_data_football, by = c('gameId' = 'gameId',
                                           'playId' = 'playId',
                                           'frameId' = 'frameId'),
            na_matches = "never")

saveRDS(tracking_data, "tracking_data_with_football.rds")

## Mutate football location to yards to end zone
## When plotting animated plays with football will actually need to make a dataframe that includes football in long format (currently only joined with defensive players)