library(tidyverse)
library(pak)
pak::pak("nflverse/nflverse")
library(nflverse)
library(nflreadr)
library(sportyR)
library(pROC)
library(googledrive)
library(ggrepel)
library(glue)
library(gganimate)
library(nflplotR)


tracking_data <- readRDS("tracking_data_with_football.rds") 
tracking_data <- tracking_data |>
  mutate(LOS_distance = abs(x-absoluteYardlineNumber) + 20, 
         football_LOS_distance = abs(football_x - absoluteYardlineNumber) - 20,## missing football location!
         custom_play_id = paste(gameId, playId),
         football_yds_to_endzone = football_x-10)




## Need to calculate distance between player and ball

#football_x reduces as it gets closer to end zone
  
## Need to mutate football location to a variable of "Yards to endzone" as, if it's getting close to the end zone, then less likely a tackle

tracking_data <- readRDS('tracking_data.rds')
tracking_data <- tracking_data |>
  mutate(LOS_distance = abs(x-absoluteYardlineNumber) + 20, ## missing football location!
         custom_play_id = paste(gameId, playId)) 


tracking_data |>
  group_by(nflId, displayName) |>
  summarise(sum_tackle = sum(tackle),
            sum_assist = sum(assist),
            sum_forced_fumble = sum(forcedFumble),
            sum_pff_missed_tackle = sum(pff_missedTackle)) |>
  ungroup() |>
  arrange(-sum_tackle) |>
  View()




tracking_data |>
  filter(displayName != "football") |>
  group_by(nflId) |>
  slice_max(s, n = 1) |>
  ungroup() |> View()|>
  slice_max(s, n = 50) |>
  View()
  
test_df_plays <- tracking_data |>
    #  filter(nflId == "46086", event == "tackle") |>
    filter(nflId == "46086" | displayName == "football", play_type == "pass")

tackle_plays <- unique(test_df_plays$custom_play_id)

test_df <- tracking_data |>
  filter(gameId == 2022100902) |>
  filter(nflId == "46086" | displayName == "football") #|>
  filter(custom_play_id %in% tackle_plays) 

  
geom_football(league = "NFL", display_range = "defense", x_trans = 60, y_trans = 26.65) +
  geom_point(data = test_df, aes(x = LOS_distance, y = y, colour = s))  +
  scale_color_viridis_c()



# Pass Tackle Modelling ---------------------------------------------------------------

## Currently tried as pass only - ROC 0.8934
test_df_plays <- tracking_data |>
  filter(play_type == "pass") |>
  #  filter(nflId == "46086", event == "tackle") |>
  #filter(nflId == "46086") |>
  mutate(tackle = replace_na(tackle, 0),
         assist = replace_na(assist, 0),
         tackle_or_assist = tackle+assist,
         tackle_or_assist = if_else(tackle_or_assist >= 1, 1, 0),
         tackle_or_assist_cat = if_else(tackle_or_assist == 1, "Tackle/Assist", "None"),
         run_gap = replace_na(run_gap, "pass"),
         dist_to_football = sqrt(((football_x - x)^2)+((football_y-y)^2))) |>
  drop_na(x,y,s,o,dir)
  

#different models for pass or run
# for run plays - run_gap
# variable for run players - offenseFormation

fit <- glm(tackle_or_assist ~ x + y + s + o + dir + yardsToGo + 
             yardlineNumber + offenseFormation + dist_to_football +
             frameId + passProbability, data = test_df_plays, family = binomial(link = 'logit'))
summary(fit)
OR <- coef(fit) %>% exp()
CI <- confint(fit) %>% exp()
cbind(OR,CI)
predpr <- predict(fit,type=c("response"))
roccurve <- roc(test_df_plays$tackle ~ predpr)
auc(roccurve)
plot(roccurve, print.auc=TRUE)


# Pass Tackle Plot Modelling ----------------------------------------------------------



model_test_df <- predict(fit, test_df_plays, type="response") %>% as.data.frame() %>% mutate_if(is.numeric, round, 2)

test_df_plays$pred_tackle <- model_test_df$.

players <- read.csv('players.csv')
  

tpa_df <- test_df_plays |>
  filter(event == "pass_arrived") |>
  group_by(gameId, playId, nflId, displayName) |>
  arrange(frameId) |>
  slice(c(1,n())) |>
  mutate(prev_tackle_prob = lag(pred_tackle),
         tackle_over_expected = tackle_or_assist-pred_tackle) |>
  slice_max(frameId, n = 1) |>
  ungroup() |>
  mutate(tackle_prob_added = pred_tackle-prev_tackle_prob) |>
  group_by(nflId, displayName) |>
  summarise(total_tackle_prob_added = sum(tackle_prob_added, na.rm = T),
            plays = n(),
            tackle_prob_added_per_play = total_tackle_prob_added/plays,
            total_TPOE = sum(tackle_over_expected, na.rm = T),
            TPOE_per_play = total_TPOE/plays,
            total_tackles_or_assists = sum(tackle_or_assist, na.rm = T)) |>
  ungroup() |>
  left_join(players, by = c('nflId', 'displayName'))

write.csv(tpa_df, "tpa_df_pass_only.csv")
drive_upload("tpa_df_pass_only.csv", name = "tpa_df_pass_only.csv", type = "spreadsheet", overwrite = TRUE)
2

tpa_df |> filter(plays >100) |> 
  ggplot() + 
  geom_point(aes(y = total_tackles_or_assists, x = total_TPOE)) + 
  geom_smooth(aes(y = total_tackles_or_assists, x = total_TPOE), se = FALSE)

tpa_df_test <- tpa_df |> filter(plays>= 100)

fit_linear_tackles <- lm(total_tackles_or_assists ~ total_TPOE, data = tpa_df_test)
summary(fit_linear_tackles) ## there's a relationship between total TPOE and total tackles, and also TPOE per play and total tackles




# Charting pass predicted tackles ----------------------------------------------



test_df <- test_df_plays |>
  filter(gameId == 2022100902) |>
  filter(nflId == "46086",
         tackle_or_assist == 1)

geom_football(league = "NFL", display_range = "defense", x_trans = 60, y_trans = 26.65) +
  geom_point(data = test_df, aes(x = LOS_distance, y = y, colour = pred_tackle))  +
  scale_color_viridis_c() +
  ggtitle("Derwin James Tackles vs LVR Week 1 2022")


# Run Tackle Modelling ---------------------------------------------------------------

## Currently tried as run only - Area under the curve: 0.6907
test_df_plays <- tracking_data |>
  filter(play_type == "run") |>
  #  filter(nflId == "46086", event == "tackle") |>
  #filter(nflId == "46086") |>
  mutate(tackle = replace_na(tackle, 0),
         assist = replace_na(assist, 0),
         tackle_or_assist = tackle+assist,
         tackle_or_assist = if_else(tackle_or_assist >= 1, 1, 0),
         tackle_or_assist_cat = if_else(tackle_or_assist == 1, "Tackle/Assist", "None"),
         run_gap = replace_na(run_gap, "pass"),
         dist_to_football = sqrt(((football_x - x)^2)+((football_y-y)^2))) |>
  drop_na(x,y,s,o,dir)


#different models for pass or run
# for run plays - run_gap
# variable for run players - offenseFormation

fit <- glm(tackle_or_assist ~ x + y + s + o + dir + yardsToGo + 
             yardlineNumber + offenseFormation + dist_to_football +
             frameId + passProbability, data = test_df_plays, family = binomial(link = 'logit'))
summary(fit)
OR <- coef(fit) %>% exp()
CI <- confint(fit) %>% exp()
cbind(OR,CI)
predpr <- predict(fit,type=c("response"))
roccurve <- roc(test_df_plays$tackle ~ predpr)
auc(roccurve) 
plot(roccurve, print.auc=TRUE)


# Run Tackle Modelling ----------------------------------------------------------



model_test_df <- predict(fit, test_df_plays, type="response") %>% as.data.frame() %>% mutate_if(is.numeric, round, 2)

test_df_plays$pred_tackle <- model_test_df$.

players <- read.csv('players.csv')


tpa_df <- test_df_plays |>
  filter(event == "handoff") |>
  group_by(gameId, playId, nflId, displayName) |>
  arrange(frameId) |>
  slice(c(1,n())) |>
  mutate(prev_tackle_prob = lag(pred_tackle),
         tackle_over_expected = tackle_or_assist-pred_tackle) |>
  slice_max(frameId, n = 1) |>
  ungroup() |>
  mutate(tackle_prob_added = pred_tackle-prev_tackle_prob) |>
  group_by(nflId, displayName) |>
  summarise(total_tackle_prob_added = sum(tackle_prob_added, na.rm = T),
            plays = n(),
            tackle_prob_added_per_play = total_tackle_prob_added/plays,
            total_TPOE = sum(tackle_over_expected, na.rm = T),
            TPOE_per_play = total_TPOE/plays,
            total_tackles_or_assists = sum(tackle_or_assist, na.rm = T)) |>
  ungroup() |>
  left_join(players, by = c('nflId', 'displayName'))

write.csv(tpa_df, "tpa_df_run_only.csv")
drive_upload("tpa_df_run_only.csv", name = "tpa_df_run_only.csv", type = "spreadsheet", overwrite = TRUE)
2

tpa_df |> filter(plays >100) |> 
  ggplot() + 
  geom_point(aes(y = total_tackles_or_assists, x = TPOE_per_play)) + 
  geom_smooth(aes(y = total_tackles_or_assists, x = TPOE_per_play), se = FALSE)

tpa_df_test <- tpa_df |> filter(plays>= 100)

fit_linear_tackles <- lm(total_tackles_or_assists ~ TPOE_per_play, data = tpa_df_test)
summary(fit_linear_tackles) ## there's a lower relationship between total TPOE and total tackles, and also TPOE per play and total tackles than for passes




# Charting run predicted tackles ----------------------------------------------



test_df <- test_df_plays |>
  filter(gameId == 2022100902) |>
  filter(nflId == "46086",
         tackle_or_assist == 1)

geom_football(league = "NFL", display_range = "defense", x_trans = 60, y_trans = 26.65) +
  geom_point(data = test_df, aes(x = LOS_distance, y = y, colour = pred_tackle))  +
  scale_color_viridis_c() +
  ggtitle("Derwin James Run Tackles vs LVR Week 1 2022")


# Predicting tackle location ----------------------------------------------

tracking_data <- readRDS("tracking_data_with_football.rds") 
tracking_data <- tracking_data |>
  mutate(LOS_distance = abs(x-absoluteYardlineNumber) + 20, 
         football_LOS_distance = abs(football_x - absoluteYardlineNumber) - 20,## missing football location!
         custom_play_id = paste(gameId, playId),
         football_yds_to_endzone = football_x-10)

tackles <- tracking_data |>
  filter(event == 'tackle') |>
  dplyr::select(gameId, playId, football_x, football_y, football_s, football_a,
                football_o) |>
  rename(tackle_football_x = football_x,
         tackle_football_y = football_y) |>
  group_by(gameId, playId) |>
  slice_max(tackle_football_y, n = 1, with_ties = FALSE) |>
  ungroup()

tracking_data <- tracking_data |>
  left_join(tackles, by = c('gameId', 'playId'), na_matches = "never")

pred_x_fit <- lm(tackle_football_x ~  football_x + football_y + football_s + yardsToGo + 
                   yardlineNumber + offenseFormation + football_LOS_distance +
                   frameId + passProbability, data = tracking_data)
summary(pred_x_fit)

pred_y_fit <- lm(tackle_football_y ~  football_x + football_y + football_s + yardsToGo + 
                   yardlineNumber + offenseFormation + football_LOS_distance +
                   frameId + passProbability, data = tracking_data)
summary(pred_y_fit)


# DB Expected Tackles -----------------------------------------------------

# Mutate position groups into CB and S
# Can I do some clustering to get different DB positions at the snap? Although I don't think pass data starts at the snap?
# Get ftn data for screens etc.
# geom_density
# Should be negative expected tackles added unless was successful having a tackle - need to look into this

tracking_data <- readRDS("tracking_data_with_football.rds") 
tracking_data <- tracking_data |>
  mutate(LOS_distance = abs(x-absoluteYardlineNumber) + 20, 
         football_LOS_distance = abs(football_x - absoluteYardlineNumber) - 20,## missing football location!
         custom_play_id = paste(gameId, playId),
         football_yds_to_endzone = football_x-10)


ftn <- nflreadr::load_ftn_charting(seasons = 2022) |>
  filter(week <= 8) |>
  dplyr::select(nflverse_game_id, nflverse_play_id, is_motion, is_play_action, is_screen_pass, is_rpo,
                n_defense_box)


pbp <- nflreadr::load_pbp(seasons = 2022) |>
  filter(week <= 8) |>
  dplyr::select(old_game_id, game_id, play_id) |>
  mutate(old_game_id = as.integer(old_game_id),
         play_id = as.numeric(play_id)) |>
  left_join(ftn, by = c('game_id' = 'nflverse_game_id', 'play_id' = 'nflverse_play_id'), na_matches = 'never') |>
  dplyr::select(-game_id)

tracking_data <- tracking_data |>
  left_join(pbp, by = c('gameId' = 'old_game_id', 'playId' = 'play_id'), na_matches = "never") |>
  mutate(position_group = case_when(position == "DB" ~ "S",
                                    position == "CB" ~ "CB",
                                    position == "FS" ~ "S",
                                    position == "SS" ~ "S",
                                    position == "ILB" ~ "LB",
                                    position == "OLB" ~ "LB",
                                    position == "MLB" ~ "LB",
                                    position == "DE" ~ "DL",
                                    position == "DT" ~ "DL",
                                    position == "NT" ~ "DL"))
  

test_df_plays <- tracking_data |>
  filter(play_type == "pass") |>
  filter(position_group == "CB" | position_group == "S") |>
  mutate(tackle = replace_na(tackle, 0),
         assist = replace_na(assist, 0),
         tackle_or_assist = tackle+assist,
         tackle_or_assist = if_else(tackle_or_assist >= 1, 1, 0),
         tackle_or_assist_cat = if_else(tackle_or_assist == 1, "Tackle/Assist", "None"),
         run_gap = replace_na(run_gap, "pass"),
         dist_to_football = sqrt(((football_x - x)^2)+((football_y-y)^2))) |>
  drop_na(x,y,s,o,dir)

## Re-do as xgboost

pass_db_fit <- glm(tackle_or_assist ~ x + y + s + o + dir + yardsToGo + 
             yardlineNumber + offenseFormation + dist_to_football + n_defense_box +
             frameId + passProbability + is_motion + is_screen_pass + is_rpo, data = test_df_plays, family = binomial(link = 'logit'))
summary(pass_db_fit)
OR <- coef(pass_db_fit) %>% exp()
CI <- exp(confint.default(pass_db_fit)) 
cbind(OR,CI)
predpr <- predict(pass_db_fit,type=c("response"))
roccurve <- roc(test_df_plays$tackle ~ predpr)
auc(roccurve) 
plot(roccurve, print.auc=TRUE)

saveRDS(pass_db_fit, "pass_db_fit.rds")

pass_db_fit <- readRDS("pass_db_fit.rds")


model_test_df <- predict(pass_db_fit, test_df_plays, type="response") %>% as.data.frame() %>% mutate_if(is.numeric, round, 2)

test_df_plays$pred_tackle <- model_test_df$.

players <- read.csv('players.csv')|>
  mutate(position_group = case_when(position == "DB" ~ "S",
                                    position == "CB" ~ "CB",
                                    position == "FS" ~ "S",
                                    position == "SS" ~ "S",
                                    position == "ILB" ~ "LB",
                                    position == "OLB" ~ "LB",
                                    position == "MLB" ~ "LB",
                                    position == "DE" ~ "DL",
                                    position == "DT" ~ "DL",
                                    position == "NT" ~ "DL"))


tpa_df <- test_df_plays |>
  filter(event == "pass_arrived" | event == "tackle" | event == "out_of_bounds" |
           event == "touchdown" | event == "fumble" | event == "pass_outcome_touchdown" |
           event == "qb_slide") |>
  group_by(gameId, playId, nflId, displayName) |>
  arrange(frameId) |>
  slice(c(1,n())) |>
  mutate(prev_tackle_prob = lag(pred_tackle),
         tackle_over_expected = tackle_or_assist-pred_tackle) |>
  slice_max(frameId, n = 1) |>
  ungroup() |>
  mutate(tackle_prob_added = pred_tackle-prev_tackle_prob) |>
  group_by(nflId, displayName) |>
  summarise(total_tackle_prob_added = sum(tackle_prob_added, na.rm = T),
            plays = n(),
            tackle_prob_added_per_play = total_tackle_prob_added/plays,
            total_TPOE = sum(tackle_over_expected, na.rm = T),
            TPOE_per_play = total_TPOE/plays,
            total_tackles_or_assists = sum(tackle_or_assist, na.rm = T)) |>
  ungroup() |>
  left_join(players, by = c('nflId', 'displayName'))

write.csv(tpa_df, "tpa_df_pass_db.csv")
drive_upload("tpa_df_pass_db.csv", name = "tpa_df_pass_db.csv", type = "spreadsheet", overwrite = TRUE)
2

tpa_df <- test_df_plays |>
  filter(is_screen_pass == "TRUE") |>
  group_by(gameId, playId, nflId, displayName) |>
  arrange(frameId) |>
  slice(c(1,n())) |>
  mutate(prev_tackle_prob = lag(pred_tackle),
         tackle_over_expected = tackle_or_assist-pred_tackle) |>
  slice_max(frameId, n = 1) |>
  ungroup() |>
  mutate(tackle_prob_added = pred_tackle-prev_tackle_prob) |>
  group_by(nflId, displayName) |>
  summarise(total_tackle_prob_added = sum(tackle_prob_added, na.rm = T),
            plays = n(),
            tackle_prob_added_per_play = total_tackle_prob_added/plays,
            total_TPOE = sum(tackle_over_expected, na.rm = T),
            TPOE_per_play = total_TPOE/plays,
            total_tackles_or_assists = sum(tackle_or_assist, na.rm = T)) |>
  ungroup() |>
  left_join(players, by = c('nflId', 'displayName'))

tpa_df |> filter(plays >100) |> 
  ggplot() + 
  geom_point(aes(y = total_tackles_or_assists, x = TPOE_per_play)) + 
  geom_smooth(aes(y = total_tackles_or_assists, x = TPOE_per_play), se = FALSE)

tpa_df_test <- tpa_df |> filter(plays>= 100)

fit_linear_tackles <- lm(total_tackles_or_assists ~ TPOE_per_play, data = tpa_df_test)
summary(fit_linear_tackles) 



# Adding offense and football back in to the code -------------------------

tracking_data_offense <- readRDS("tracking_data_offense.rds") |>
  filter(play_type == "pass") |>
  mutate(is_ball_carrier = case_when(nflId == ballCarrierId ~ 1,
                                     nflId != ballCarrierId ~ 0)) |>
  filter(is_ball_carrier == 1) |>
  dplyr::select(-is_ball_carrier)
tracking_data_football <- readRDS("tracking_data_football.rds") |>
  filter(play_type == "pass") 

colours <- nflreadr::load_teams() |>
  dplyr::select(team_abbr, team_color, team_color2)

test_df_plays <- bind_rows(test_df_plays, tracking_data_offense, tracking_data_football) |>
  left_join(colours, by = c('club' = 'team_abbr'))

2022100911
3217

gameId_filter <- "2022100911"
playId_filter <- "3217"

test_df_plays_plot_animate <- test_df_plays |>
  filter(gameId == gameId_filter, playId == playId_filter) |>
  mutate(position = case_when(displayName == "football" ~ "the",
                              TRUE ~ position),
         name_position = str_c(position, " ", displayName),
         pred_tackle = case_when(defensive_player == "N" ~ 0,
                                 displayName == "football" ~ 0,
                                 TRUE ~ pred_tackle),
         team_color = case_when(defensive_player == "N" ~ "pink",
                                displayName == "football" ~ "red",
                                TRUE ~ team_color),
         team_color2 = case_when(defensive_player == "N" ~ "pink",
                                displayName == "football" ~ "red",
                                TRUE ~ team_color2))

test_df_plays_defense <- test_df_plays |>
  filter(defensive_player == "Y") |>
  filter(gameId == gameId_filter, playId == playId_filter) |>
  mutate(position = case_when(displayName == "football" ~ "the",
                              TRUE ~ position),
         name_position = str_c(position, " ", displayName),
         pred_tackle = case_when(defensive_player == "N" ~ 0,
                                 displayName == "football" ~ 0,
                                 TRUE ~ pred_tackle),
         team_color = case_when(defensive_player == "N" ~ "pink",
                                displayName == "football" ~ "red",
                                TRUE ~ team_color),
         team_color2 = case_when(defensive_player == "N" ~ "pink",
                                 displayName == "football" ~ "red",
                                 TRUE ~ team_color2))

test_df_plays_defense_title <- test_df_plays |>
  filter(defensive_player == "Y") |>
  filter(gameId == gameId_filter, playId == playId_filter) |>
  mutate(position = case_when(displayName == "football" ~ "the",
                              TRUE ~ position),
         name_position = str_c(position, " ", displayName),
         pred_tackle = case_when(defensive_player == "N" ~ 0,
                                 displayName == "football" ~ 0,
                                 TRUE ~ pred_tackle),
         team_color = case_when(defensive_player == "N" ~ "pink",
                                displayName == "football" ~ "red",
                                TRUE ~ team_color),
         team_color2 = case_when(defensive_player == "N" ~ "pink",
                                 displayName == "football" ~ "red",
                                 TRUE ~ team_color2)) 

test_df_plays_offense <- test_df_plays |>
  filter(defensive_player == "N") |>
  filter(gameId == gameId_filter, playId == playId_filter) |>
  mutate(position = case_when(displayName == "football" ~ "the",
                              TRUE ~ position),
         name_position = str_c(position, " ", displayName),
         pred_tackle = case_when(defensive_player == "N" ~ 0,
                                 displayName == "football" ~ 0,
                                 TRUE ~ pred_tackle),
         team_color = case_when(defensive_player == "N" ~ "pink",
                                displayName == "football" ~ "red",
                                TRUE ~ team_color),
         team_color2 = case_when(defensive_player == "N" ~ "pink",
                                 displayName == "football" ~ "red",
                                 TRUE ~ team_color2))

test_df_plays_football <- test_df_plays |>
  filter(displayName == "football") |>
  filter(gameId == gameId_filter, playId == playId_filter) |>
  mutate(position = case_when(displayName == "football" ~ "the",
                              TRUE ~ position),
         name_position = str_c(position, " ", displayName),
         pred_tackle = case_when(defensive_player == "N" ~ 0,
                                 displayName == "football" ~ 0,
                                 TRUE ~ pred_tackle),
         team_color = case_when(defensive_player == "N" ~ "pink",
                                displayName == "football" ~ "red",
                                TRUE ~ team_color),
         team_color2 = case_when(defensive_player == "N" ~ "pink",
                                 displayName == "football" ~ "red",
                                 TRUE ~ team_color2))

test_df_plays_plot <- test_df_plays |>
  filter(gameId == gameId_filter, playId == playId_filter) |>
  mutate(position = case_when(displayName == "football" ~ "the",
                              TRUE ~ position),
         name_position = str_c(position, " ", displayName),
         pred_tackle = case_when(defensive_player == "N" ~ 0,
                                 displayName == "football" ~ 0,
                                 TRUE ~ pred_tackle),
         team_color = case_when(defensive_player == "N" ~ "pink",
                                displayName == "football" ~ "red",
                                TRUE ~ team_color),
         team_color2 = case_when(defensive_player == "N" ~ "pink",
                                 displayName == "football" ~ "red",
                                 TRUE ~ team_color2))

test_df_plays_plot_title <- test_df_plays_plot_animate |>
  filter(gameId == gameId_filter, playId == playId_filter) |>
  slice_min(frameId, n = 1, with_ties = FALSE)


# Need to get the football position & the ball carrier back into the play
# Just filter for the football and the offensive player from tracking data then bind rows to above? & mutate predicted tackle to 0.00 for them?
geom_football(league = "NFL", x_trans = 60, y_trans = 26.65) +
  geom_point(data = test_df_plays_offense, aes(x = x, y = y), colour = "pink", alpha = 0.8) +
  geom_point(data = test_df_plays_football, aes(x = x, y = y), colour = "red", alpha = 0.8) +
  geom_point(data = test_df_plays_defense, aes(x = x, y = y, fill = pred_tackle), shape = 21) +
  geom_label_repel(data = test_df_plays_defense |> slice(2L), aes(x = x, y = y, label = name_position,
                                                                               colour = club),
                   alpha = 0.9) +
  geom_label_repel(data = test_df_plays_offense |> slice_min(frameId, n = 1), aes(x = x, y = y, label = name_position,
                                                                               colour = club),
                   alpha = 0.9) +
  geom_label_repel(data = test_df_plays_football |> slice_min(frameId, n = 1), aes(x = x, y = y, label = name_position,
                                                                                  colour = club),
                   alpha = 0.9) +
  scale_fill_viridis_c() +
  scale_colour_nfl(type = "primary") +
  ggtitle(glue({test_df_plays_plot_title$gameId}, " Qtr ", {test_df_plays_plot_title$quarter},
               " Down ", {test_df_plays_plot_title$down}, " Play ", {test_df_plays_plot_title$playDescription}),
          subtitle = "Chart & Model: @TAlbTree") 



## gganimate version - mutate jersey numbers to be "Team - Jersey Number"

animation <- geom_football(league = "NFL", x_trans = 60, y_trans = 26.65) +
  geom_label(data = test_df_plays_defense, aes(x = x, y = y, label = displayName), colour = test_df_plays_defense$team_color,
             alpha = 0.9, nudge_y = 4) +
  geom_label(data = test_df_plays_football, aes(x = x, y = y, label = displayName), fill = "red",
             alpha = 0.4, nudge_y = 4) +
  geom_label(data = test_df_plays_offense, aes(x = x, y = y, label = displayName), fill = "pink",
             alpha = 0.9, nudge_y = 4) +
  geom_point(data = test_df_plays_defense, aes(x = x, y = y, fill = pred_tackle), shape = 21, size = 6) +
  geom_point(data = test_df_plays_offense, aes(x = x, y = y), fill = "pink", alpha = 0.8, shape = 21, size = 6) +
  geom_point(data = test_df_plays_football, aes(x = x, y = y), fill = "red", alpha = 0.8, shape = 21, size = 6) +
  scale_fill_viridis_c() + 
  ggtitle(glue({test_df_plays_plot_title$gameId}, " Qtr ", {test_df_plays_plot_title$quarter}," - ", {test_df_plays_plot_title$playDescription}),
          subtitle = "Chart & Model: @TAlbTree") +
  theme(title = element_text(size = 8)) +
  transition_time(frameId) +
  ease_aes('linear')

animate(animation, width = 1000, height = 650, res = 150, fps = 20)
anim_save(glue("plots/{test_df_plays_plot_title$gameId}_{test_df_plays_plot_title$playId}_{test_df_plays_plot_title$ballCarrierDisplayName}.gif"), 
          dpi = 1000, width = 1000, height = 650)



ggplot(test_df_plays_plot_animate) +
  geom_point(aes(x = x, y = y, fill = pred_tackle, colour = team_color), shape = 21) +
  geom_label(aes(x = x, y = y, label = displayName, fill = pred_tackle,
                 colour = team_color),
                   alpha = 0.9) +
  scale_fill_viridis_c() +
  #ggtitle(glue({test_df_plays_plot_animate$gameId}, " Qtr ", {test_df_plays_plot_animate$quarter},
             #  " Down ", {test_df_plays_plot_animate$down}, " Play ", {test_df_plays_plot_animate$playDescription}),
         # subtitle = "Chart & Model: @TAlbTree") #+
  #scale_colour_nfl(type = "primary") +
  transition_time(frameId) +
  ease_aes('linear')
