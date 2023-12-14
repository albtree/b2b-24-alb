
# Collate Tracking Data ---------------------------------------------------


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
         defensive_player = if_else(defensive_player == "Y", "Y", "N"))

plays <- read.csv('plays.csv')

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


tracking_data_football <- readRDS('tracking_data_football.rds')
tracking_data_football <- tracking_data_football |>
  dplyr::select(gameId, playId, displayName, frameId, time,
                x, y, s, a, dis, o, dir) |>
  rename(football_name = displayName, football_time = time,
         football_x = x, football_y = y, football_s = s,
         football_a = a, football_dis = dis, football_o = o,
         football_dir = dir)

tracking_data <- readRDS('tracking_data_defense.rds') 
tracking_data <- tracking_data |>
  left_join(tracking_data_football, by = c('gameId' = 'gameId',
                                           'playId' = 'playId',
                                           'frameId' = 'frameId'),
            na_matches = "never")

saveRDS(tracking_data, "tracking_data_with_football.rds")


# Pull in data  ---------------------------------------------------------------------

library(hrbrthemes)
library(ggthemes)
library(pak)
library(gt)
library(gtExtras)
library(nflverse)
library(nflreadr)
library(sportyR)
library(pROC)
library(googledrive)
library(ggrepel)
library(glue)
library(gganimate)
library(nflplotR)
library(vip)
library(car)

players_offense <- read.csv("players.csv") |> # Pull in Player dataset
  filter(position == "WR" | position == "QB" | position == "TE" | position == "RB") |>
  dplyr::select(nflId, position) |>
  rename(ballcarrier_position = position)

tracking_data <- readRDS("tracking_data_with_football.rds")  #Pull in Tracking Data wrangled from earlier
tracking_data <- tracking_data |>
  mutate(LOS_distance = abs(x-absoluteYardlineNumber) + 20, 
         football_LOS_distance = abs(football_x - absoluteYardlineNumber) - 20,
         custom_play_id = paste(gameId, playId),
         football_yds_to_endzone = football_x-10) 

ftn <- nflreadr::load_ftn_charting(seasons = 2022) |> #Pull in FTN Data from nflverse
  filter(week <= 8) |>
  dplyr::select(nflverse_game_id, nflverse_play_id, is_motion, is_play_action, is_screen_pass, is_rpo,
                n_defense_box)


pbp <- nflreadr::load_pbp(seasons = 2022) |> #Pull in play by play data from nflverse
  filter(week <= 8) |>
  dplyr::select(old_game_id, game_id, play_id, week) |>
  mutate(old_game_id = as.integer(old_game_id),
         play_id = as.numeric(play_id)) |>
  left_join(ftn, by = c('game_id' = 'nflverse_game_id', 'play_id' = 'nflverse_play_id'), na_matches = 'never') |>
  dplyr::select(-game_id)

tracking_data <- tracking_data |> #Make position groups
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
                                    position == "NT" ~ "DL")) |>
  left_join(players_offense, by = c('ballCarrierId' = 'nflId'))


# Get dataset ready for modelling -----------------------------------------



  test_df_plays <- tracking_data |>
    filter(play_type == "pass") |> #Filter by pass plays only
    filter(position_group == "CB" | position_group == "S") |> #Filter by defensive backs only
    filter(ballcarrier_position == "WR") |> #Filter by Wide Receivers only
    mutate(tackle = replace_na(tackle, 0),
           assist = replace_na(assist, 0),
           tackle_or_assist = tackle+assist, #Classify Solo Tackle or Assisted tackle both as a successful tackle
           tackle_or_assist = if_else(tackle_or_assist >= 1, 1, 0),
           tackle_or_assist_cat = if_else(tackle_or_assist == 1, "Tackle/Assist", "None"), 
           o = case_when(playDirection == "left" ~ abs(o-180),
                         TRUE ~ o),
           dir = case_when(playDirection == "left" ~ abs(dir - 180),
                           TRUE ~ dir),
           dist_to_football = sqrt(((football_x - x)^2)+((football_y-y)^2))) |> ## Calculate distance from player to football
    drop_na(x,y,s,o,dir) 

test_df_train <- test_df_plays |> #Split train/test data
  filter(week <= 6)

test_df_test <- test_df_plays |> #split train/test data
  filter(week >6)


# Logistic Regression Model -----------------------------------------------



pass_db_fit <- glm(tackle_or_assist ~ y + s + a + yardsToGo + dir + o +
                     yardlineNumber + dist_to_football + passProbability + is_motion + is_screen_pass, data = test_df_train, family = binomial(link = 'logit')) #fit logistic regression model
summary(pass_db_fit)
back_db_fit <- MASS::stepAIC(pass_db_fit, direction = "both", trace = TRUE) ## Bi-directional stepwise regression
summary(back_db_fit)
vip(pass_db_fit) +
  labs(title = "Train Data - Weeks 1:6 - Variable Importance Plot") #Variable importance plot
car::vif(pass_db_fit) ## check multicollinearity
OR <- coef(pass_db_fit) %>% exp()
CI <- exp(confint.default(pass_db_fit)) 
cbind(OR,CI)
predpr <- predict(pass_db_fit, test_df_train, type=c("response"))
roccurve <- roc(test_df_train$tackle_or_assist ~ predpr)
auc(roccurve) ## Train AUC 0.884
plot(roccurve, print.auc=TRUE,
     main = "Train Data - Weeks 1 to 6")


saveRDS(pass_db_fit, "pass_db_fit.rda")

pass_db_fit <- readRDS("pass_db_fit.rda")

predpr <- predict(pass_db_fit, test_df_test, type = c("response"))
roccurve <- roc(test_df_test$tackle_or_assist ~ predpr)
auc(roccurve) ## Test AUC 0.892 - better than traing data
plot(roccurve, print.auc = TRUE,
     main = "Test Data - Weeks 7 and 8")

# Apply model to full data set --------------------------------------------

model_test_df <- predict(pass_db_fit, test_df_plays, type="response") %>% as.data.frame() %>% mutate_if(is.numeric, round, 2)

test_df_plays$pred_tackle <- model_test_df$.

saveRDS(test_df_plays, "test_df_plays.rds")


# Summarise Expected Tackle data for DBs ----------------------------------

test_df_plays <- readRDS("test_df_plays.rds")


rosters <- nflreadr::load_rosters(seasons = 2022) |>
  mutate(depth_chart_position = case_when(gsis_id == "00-0036356" ~ "SS",
                                          gsis_id == "00-0037661" ~ "CB",
                                          TRUE ~ depth_chart_position)) |>
  filter(depth_chart_position == "SS" | depth_chart_position == "FS" | depth_chart_position == "CB" | depth_chart_position == "DB") |>
  mutate(merge_name = clean_player_names(full_name, lowercase = TRUE)) |>
  mutate(position_group = case_when(depth_chart_position == "DB" ~ "S",
                                    depth_chart_position == "CB" ~ "CB",
                                    depth_chart_position == "FS" ~ "S",
                                    depth_chart_position == "SS" ~ "S",
                                    depth_chart_position == "ILB" ~ "LB",
                                    depth_chart_position == "OLB" ~ "LB",
                                    depth_chart_position == "MLB" ~ "LB",
                                    depth_chart_position == "DE" ~ "DL",
                                    depth_chart_position == "DT" ~ "DL",
                                    depth_chart_position == "NT" ~ "DL")) |> 
  dplyr::select(team, position_group, full_name, gsis_id, merge_name)

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
                                    position == "NT" ~ "DL")) |>
  mutate(merge_name = clean_player_names(displayName, lowercase = TRUE)) |>
  left_join(rosters, by = c('merge_name', 'position_group'))


tpa_df <- test_df_plays |>
  filter(event == "pass_arrived" | event == "tackle" | event == "out_of_bounds" |
           event == "touchdown" | event == "fumble" | event == "pass_outcome_touchdown" |
           event == "qb_slide") |>
  group_by(gameId, playId, nflId, displayName) |>
  arrange(frameId) |>
  dplyr::slice(c(1,n())) |>
  mutate(prev_tackle_prob = lag(pred_tackle),
         tackle_over_expected = tackle_or_assist-pred_tackle) |>
  slice_max(frameId, n = 1) |>
  ungroup() |>
  mutate(tackle_prob_added = pred_tackle-prev_tackle_prob) |>
  group_by(nflId, club, displayName) |>
  summarise(total_tackle_prob_added = sum(tackle_prob_added, na.rm = T),
            plays = n(),
            tackle_prob_added_per_play = total_tackle_prob_added/plays,
            total_TPOE = sum(tackle_over_expected, na.rm = T),
            TPOE_per_play = total_TPOE/plays,
            total_tackles_or_assists = sum(tackle_or_assist, na.rm = T),
            tackles_per_play = total_tackles_or_assists/plays) |>
  ungroup() |> 
  left_join(players, by = c('nflId', 'displayName'))|>
  drop_na(position_group) |>
  mutate_if(is.numeric, round, 3) 

write.csv(tpa_df, "tpa_df_pass_db.csv")
drive_upload("tpa_df_pass_db.csv", name = "tpa_df_pass_db.csv", type = "spreadsheet", overwrite = TRUE)
2

tpa_df <- read.csv("tpa_df_pass_db.csv") 


tpa_df_tackles_added <- test_df_plays |> ## Dataframe to check which plays had the highest tackle probability added
  filter(event == "pass_arrived" | event == "tackle" | event == "out_of_bounds" |
           event == "touchdown" | event == "fumble" | event == "pass_outcome_touchdown" |
           event == "qb_slide") |>
  group_by(gameId, playId, nflId, displayName) |>
  arrange(frameId) |>
  dplyr::slice(c(1,n())) |> # Get the first and last frame of the play
  mutate(prev_tackle_prob = lag(pred_tackle),
         tackle_over_expected = tackle_or_assist-pred_tackle) |>
  slice_max(frameId, n = 1, with_ties = FALSE) |>
  ungroup() |>
  mutate(tackle_prob_added = pred_tackle-prev_tackle_prob) |>
  dplyr::select(tackle_prob_added, tackle_over_expected, everything()) 


tpa_df_tackles_added2 <- test_df_plays |> ## Dataframe to check which plays had the highest tackle probability added
  group_by(gameId, playId, nflId, displayName) |>
  slice_max(pred_tackle, n = 1, with_ties = FALSE) |>
  mutate(tackle_over_expected2 = tackle_or_assist-pred_tackle) |>
  ungroup() |>
  dplyr::select(tackle_over_expected2, everything()) 


tpa_df_tackle_avoided_by_play <- test_df_plays_short |> # Dataframe so can more easily the highest scoring plays by expected tackles avoided
  group_by(gameId, playId, nflId) |>
  slice_max(pred_tackle, n = 1, with_ties = FALSE) |>
  mutate(avoided_tackle = pred_tackle-tackle_or_assist) |>
  ungroup() |>
  group_by(ballCarrierId, ballCarrierDisplayName, gameId, playId) |>
  summarise(total_tackle_avoided = sum(avoided_tackle, na.rm = T),) |>
  ungroup() |>
  left_join(players, by = c('ballCarrierId' = 'nflId', 'ballCarrierDisplayName' = 'displayName')) |>
  mutate_if(is.numeric, round, 3)


# DB Expected Tackle leaderboards and scatter plot ------------------------

tpa_df |>
  filter(plays >= 75, position != "football") |>
  select(displayName, gsis_id, position, team, TPOE_per_play, tackle_prob_added_per_play, tackles_per_play, plays, total_tackles_or_assists) |>
  slice_max(TPOE_per_play, n = 15, with_ties = FALSE) |>
  gt() |>
  tab_header(
    title = "Top 15 DBs by Defensive Back Tackles over Expected per Pass Play",
    subtitle = "Weeks 1:8 2022. Minimum 75 pass snaps"
  ) |>
  cols_align(
    "center",
    columns = c(displayName, gsis_id, position, team, TPOE_per_play, tackle_prob_added_per_play, tackles_per_play, plays, total_tackles_or_assists)
  ) |> 
  cols_label(
    displayName = "Player",
    position = "Pos.",
    gsis_id = "",
    team = "Team",
    TPOE_per_play = "TOE/play",
    tackle_prob_added_per_play = "TPA/play",
    tackles_per_play = "Tackles/play",
    plays = "Plays",
    total_tackles_or_assists = "Total Comb. Tackles",
  ) |>
  tab_source_note(
    source_note = "Metric developed for #BigDataBowl by @TAlbTree") |>
  tab_source_note(
    source_note = "TOE = Tackles over Expected"
  ) |>
  tab_source_note(
    source_note = "TPA = Tackle Probability Added"
  ) |>
  tab_source_note(
    source_note = "Table: @TAlbTree"
  ) |>
  tab_source_note(
    source_note = "Data: NFL via BigDataBowl, nflverse"
  ) |>
  tab_options(footnotes.font.size = 12) |>
  gt_nfl_headshots("gsis_id", height = 35) |>
  gt_nfl_wordmarks("team") |>
  gt_hulk_col_numeric(c(TPOE_per_play,tackle_prob_added_per_play, tackles_per_play, plays, total_tackles_or_assists)) |>
  gt_theme_538() |>
  gtsave("non_animate_plots/DB_TOE.png")

tpa_df |>
  filter(plays >= 75, position != "football") |>
  select(displayName, gsis_id, position, team, tackle_prob_added_per_play, TPOE_per_play, tackles_per_play, plays, total_tackles_or_assists) |>
  slice_max(tackle_prob_added_per_play, n = 15, with_ties = FALSE) |>
  gt() |>
  tab_header(
    title = "Top 15 DBs by Defensive Back Tackle Probability Added per Pass Play",
    subtitle = "Weeks 1:8 2022. Minimum 75 pass snaps"
  ) |>
  cols_align(
    "center",
    columns = c(displayName, gsis_id, position, team, tackle_prob_added_per_play, TPOE_per_play, tackles_per_play, plays, total_tackles_or_assists)
  ) |> 
  cols_label(
    displayName = "Player",
    position = "Pos.",
    gsis_id = "",
    team = "Team",
    tackle_prob_added_per_play = "TPA/play",
    TPOE_per_play = "TOE/play",
    tackles_per_play = "Tackles/play",
    plays = "Plays",
    total_tackles_or_assists = "Total Comb. Tackles",
  ) |>
  tab_source_note(
    source_note = "Metric developed for #BigDataBowl by @TAlbTree") |>
  tab_source_note(
    source_note = "TOE = Tackles over Expected"
  ) |>
  tab_source_note(
    source_note = "TPA = Tackle Probability Added"
  ) |>
  tab_source_note(
    source_note = "Table: @TAlbTree"
  ) |>
  tab_source_note(
    source_note = "Data: NFL via BigDataBowl, nflverse"
  ) |>
  tab_options(footnotes.font.size = 12) |>
  gt_nfl_headshots("gsis_id", height = 35) |>
  gt_nfl_wordmarks("team") |>
  gt_hulk_col_numeric(c(TPOE_per_play,tackle_prob_added_per_play, tackles_per_play, plays, total_tackles_or_assists)) |>
  gt_theme_538() |>
  gtsave("non_animate_plots/DB_TPA.png")


tpa_df |>
  filter(plays >= 50) |>
  ggplot(aes(y = tackles_per_play,
             x = TPOE_per_play)) +
  geom_point() +
  geom_smooth(se = FALSE) +
  ggpubr::stat_regline_equation(label.y.npc = "top", label.x.npc = "left",
                                aes(label = ..rr.label..)) +
  theme_ipsum() +
  labs(title = "Tackles over Expected/Play has a strong correlation to Tackles/Play",
       subtitle = "Minimum 50 plays",
       x = "Tackles Over Expected/Play",
       y = "Tackles/Play",
       caption = "Model & Chart: @TAlbTree") +
  theme(plot.title = element_text(size = 10))
ggsave(glue("non_animate_plots/TPOE_vs_tackles_per_play.png"), bg = "#ffffff", dpi = 1000, width = 10, height = 6)

tpa_df |>
  filter(plays >= 50) |>
  ggplot(aes(y = tackles_per_play,
             x = tackle_prob_added_per_play)) +
  geom_point() +
  geom_smooth(se = FALSE) +
  ggpubr::stat_regline_equation(label.y.npc = "top", label.x.npc = "left",
                                aes(label = ..rr.label..)) +
  theme_ipsum() +
  labs(title = "Tackle Probability Added per Play is not correlated with Tackles/Play",
       subtitle = "Minimum 50 plays",
       x = "Tackle Probability Added/Play",
       y = "Tackles/Play",
       caption = "Model & Chart: @TAlbTree") +
  theme(plot.title = element_text(size = 10))
ggsave(glue("non_animate_plots/TPA_vs_tackles_per_play.png"), bg = "#ffffff", dpi = 1000, width = 10, height = 6)


# Offense Avoided Tackles -------------------------------------------------


test_df_plays <- readRDS("test_df_plays.rds")

test_df_plays_short <- test_df_plays |> ## Adjusted dataframe so easier to view
  dplyr::select(gameId, playId, frameId, time, playDescription, event, ballCarrierId,
                ballCarrierDisplayName, nflId, displayName, club, position, tackle_or_assist,
                pred_tackle)

rosters <- nflreadr::load_rosters(seasons = 2022) |> ## Pull in rosters from nflverse
  filter(position == "RB" | position == "WR" | position == "TE" | position == "QB" | position == "FB") |>
  mutate(merge_name = clean_player_names(full_name, lowercase = TRUE)) |>
  mutate(merge_name = case_when(gsis_id == "00-0032688" ~ "robbie chosen", ## Fix Robbie Anderson.....
                                TRUE ~ merge_name)) |>
  dplyr::select(team, depth_chart_position, full_name, gsis_id, merge_name)

player_stats <- nflreadr::load_player_stats(seasons = 2022) |> ## Pull in player stats from nflverse
  filter(week<=8) |>
  group_by(player_id, player_display_name) |>
  summarise_if(is.numeric, sum, na.rm = T) |>
  ungroup()

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
                                    position == "NT" ~ "DL"))|>
  filter(position == "RB" | position == "WR" | position == "TE" | position == "QB" | position == "FB") |>
  mutate(merge_name = clean_player_names(displayName, lowercase = TRUE)) |>
  left_join(rosters, by = c('merge_name', 'position' = 'depth_chart_position'))

num_plays_bc <- test_df_plays_short |> # Get number of plays (catches) for the ball carrier 
  group_by(gameId, playId) |>
  slice_max(frameId, n = 1, with_ties = FALSE) |>
  ungroup() |>
  group_by(ballCarrierId, ballCarrierDisplayName) |>
  summarise(plays = n()) |>
  ungroup() |>
  left_join(players, by = c('ballCarrierId' = 'nflId', 'ballCarrierDisplayName' = 'displayName')) |>
  dplyr::select(ballCarrierId, plays)

tpa_df_tackle_avoided <- test_df_plays_short |> # Dataframe with trackiing data and expected tackle model predictions
  group_by(gameId, playId, nflId) |> # Group by gameId, playId, and defensive player Id
  slice_max(pred_tackle, n = 1, with_ties = FALSE) |> # Slice by maximal predicted tackle for each defensive player on that play
  mutate(avoided_tackle = pred_tackle-tackle_or_assist) |> # Calculate avoided tackle
  ungroup() |>
  group_by(ballCarrierId, ballCarrierDisplayName) |> # Group by ball carrier
  summarise(total_tackle_avoided = sum(avoided_tackle, na.rm = T),) |> # Get sum of avoided tackles
  ungroup() |>
  left_join(players, by = c('ballCarrierId' = 'nflId', 'ballCarrierDisplayName' = 'displayName')) |>
  left_join(num_plays_bc, by = c('ballCarrierId')) |> # Pull in number of plays for player in dataset
  mutate(tackles_avoided_per_play = total_tackle_avoided/plays) |>
  left_join(player_stats, by = c('gsis_id' = 'player_id')) |> # Pull in NFL stats e.g. Receiving Yards, YAC etc.
  mutate(yac_per_play = receiving_yards_after_catch/plays) |>
  mutate_if(is.numeric, round, 3)

write.csv(tpa_df_tackle_avoided, "tackles_avoided.csv")
drive_upload("tackles_avoided.csv", name = "tackles_avoided.csv", type = "spreadsheet", overwrite = TRUE)
2

tpa_df_tackle_avoided_by_play <- test_df_plays_short |> # Dataframe so can more easily the highest scoring plays by expected tackles avoided
  group_by(gameId, playId, nflId) |>
  slice_max(pred_tackle, n = 1, with_ties = FALSE) |>
  mutate(avoided_tackle = pred_tackle-tackle_or_assist) |>
  ungroup() |>
  group_by(ballCarrierId, ballCarrierDisplayName, gameId, playId) |>
  summarise(total_tackle_avoided = sum(avoided_tackle, na.rm = T),) |>
  ungroup() |>
  left_join(players, by = c('ballCarrierId' = 'nflId', 'ballCarrierDisplayName' = 'displayName')) |>
  mutate_if(is.numeric, round, 3)

write.csv(tpa_df_tackle_avoided_by_play, "tpa_df_tackle_avoided_by_play.csv")
drive_upload("tpa_df_tackle_avoided_by_play.csv", name = "tpa_df_tackle_avoided_by_play.csv", type = "spreadsheet", overwrite = TRUE)
2

# Tackle avoided plots ----------------------------------------------------------------

tpa_df_tackle_avoided |>
  ggplot(aes(x = total_tackle_avoided,
             y = receiving_yards_after_catch)) +
  geom_point() +
  geom_smooth(se = FALSE) +
  ggpubr::stat_regline_equation(label.y.npc = "top", label.x.npc = "left",
                                aes(label = ..rr.label..)) +
  theme_ipsum() +
  labs(title = "Receiving Yards after Catch are closely correlated with DB Expected Tackles Avoided",
       x = "Total DB Expected Tackles Avoided",
       y = "Receiving Yards After Catch",
       caption = "Model & Chart: @TAlbTree") +
  theme(plot.title = element_text(size = 10))
ggsave(glue("non_animate_plots/xta_vs_yac.png"), bg = "#ffffff", dpi = 1000, width = 10, height = 6)

tpa_df_tackle_avoided |>
  filter(plays >= 25, position == "WR") |>
  select(ballCarrierDisplayName, gsis_id, team, tackles_avoided_per_play, yac_per_play, plays, total_tackle_avoided, receiving_yards_after_catch) |>
  slice_max(tackles_avoided_per_play, n = 15, with_ties = FALSE) |>
  gt() |>
  tab_header(
    title = "Top 15 WRs by Defensive Back Expected Tackles Avoided per Pass Play",
    subtitle = "Weeks 1:8 2022. Minimum 25 plays (catches)"
  ) |>
  cols_align(
    "center",
    columns = c(ballCarrierDisplayName, gsis_id, team, tackles_avoided_per_play, yac_per_play, plays, total_tackle_avoided, receiving_yards_after_catch)
  ) |> 
  cols_label(
    ballCarrierDisplayName = "Player",
    gsis_id = "",
    team = "Team",
    tackles_avoided_per_play = "xTA/play",
    yac_per_play = "YAC/play",
    plays = "Plays",
    total_tackle_avoided = "Total xTA",
    receiving_yards_after_catch = "YAC"
  ) |>
  tab_source_note(
    source_note = "Metric developed for #BigDataBowl by @TAlbTree") |>
  tab_source_note(
    source_note = "xTA = Expected Tackles Avoided"
  ) |>
  tab_source_note(
    source_note = "YAC = Yards After Catch"
  ) |>
  tab_source_note(
    source_note = "Table: @TAlbTree"
  ) |>
  tab_source_note(
    source_note = "Data: NFL via BigDataBowl, nflverse"
  ) |>
  tab_options(footnotes.font.size = 12) |>
  gt_nfl_headshots("gsis_id", height = 35) |>
  gt_nfl_wordmarks("team") |>
  gt_hulk_col_numeric(c(tackles_avoided_per_play, yac_per_play, plays, total_tackle_avoided, receiving_yards_after_catch)) |>
  gt_theme_538() |>
  gtsave("non_animate_plots/WR_XTA.png")


tpa_df_tackle_avoided |>
  filter(plays >= 25, position == "WR") |>
  select(ballCarrierDisplayName, gsis_id, team, tackles_avoided_per_play, yac_per_play, plays, total_tackle_avoided, receiving_yards_after_catch) |>
  slice_min(tackles_avoided_per_play, n = 15, with_ties = FALSE) |>
  gt() |>
  tab_header(
    title = "Worst 15 WRs by Defensive Back Expected Tackles Avoided per Pass Play",
    subtitle = "Weeks 1:8 2022. Minimum 25 plays (catches)"
  ) |>
  cols_align(
    "center",
    columns = c(ballCarrierDisplayName, gsis_id, team, tackles_avoided_per_play, yac_per_play, plays, total_tackle_avoided, receiving_yards_after_catch)
  ) |> 
  cols_label(
    ballCarrierDisplayName = "Player",
    gsis_id = "",
    team = "Team",
    tackles_avoided_per_play = "xTA/play",
    yac_per_play = "YAC/play",
    plays = "Plays",
    total_tackle_avoided = "Total xTA",
    receiving_yards_after_catch = "YAC"
  ) |>
  tab_source_note(
    source_note = "Metric developed for #BigDataBowl by @TAlbTree") |>
  tab_source_note(
    source_note = "xTA = Expected Tackles Avoided"
  ) |>
  tab_source_note(
    source_note = "YAC = Yards After Catch"
  ) |>
  tab_source_note(
    source_note = "Table: @TAlbTree"
  ) |>
  tab_source_note(
    source_note = "Data: NFL via BigDataBowl, nflverse"
  ) |>
  tab_options(footnotes.font.size = 12) |>
  gt_nfl_headshots("gsis_id", height = 35) |>
  gt_nfl_wordmarks("team") |>
  gt_hulk_col_numeric(c(tackles_avoided_per_play, yac_per_play, plays, total_tackle_avoided, receiving_yards_after_catch)) |>
  gt_theme_538() |>
  gtsave("non_animate_plots/WR_XTA_worst.png")

# Adding offense and football back in to the code pre animated plots  -------------------------

test_df_plays <- readRDS("test_df_plays.rds")

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

test_df_viz <- bind_rows(test_df_plays, tracking_data_offense, tracking_data_football) |>
  left_join(colours, by = c('club' = 'team_abbr'))


# Animated plots ----------------------------------------------------------



gameId_filter <- "2022092512" #repeat for 2022100300
playId_filter <- "1212" #repeat for 1547

test_df_plays_plot_animate <- test_df_viz |>
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

test_df_plays_defense <- test_df_viz |> ## defensive players location data for animated plot 
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

test_df_plays_offense <- test_df_viz |> ## offensive players location data for animated plot
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

test_df_plays_football <- test_df_viz |> ## football location data for animated plot
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

test_df_plays_plot_title <- test_df_plays_plot_animate |> ## For title
  filter(gameId == gameId_filter, playId == playId_filter) |>
  slice_min(frameId, n = 1, with_ties = FALSE)

animation <- geom_football(league = "NFL", x_trans = 60, y_trans = 26.65) + ## Animated plot
  geom_label(data = test_df_plays_defense, aes(x = x, y = y, label = displayName), colour = test_df_plays_defense$team_color,
             alpha = 0.9, nudge_y = 4) +
  geom_label(data = test_df_plays_football, aes(x = x, y = y, label = displayName), fill = "red",
             alpha = 0.4, nudge_y = 4) +
  geom_label(data = test_df_plays_offense, aes(x = x, y = y, label = displayName), fill = "pink",
             alpha = 0.9, nudge_y = 4) +
  geom_point(data = test_df_plays_defense, aes(x = x, y = y, fill = pred_tackle), shape = 21, size = 6) +
  geom_point(data = test_df_plays_offense, aes(x = x, y = y), fill = "pink", alpha = 0.8, shape = 21, size = 6) +
  geom_point(data = test_df_plays_football, aes(x = x, y = y), fill = "red", alpha = 0.8, shape = 21, size = 6) +
  scale_fill_viridis_c(limits=c(0, 1)) + 
  labs(title = glue({test_df_plays_plot_title$gameId}, " Qtr ", {test_df_plays_plot_title$quarter}," - ", {test_df_plays_plot_title$possessionTeam}, " vs ",
                    {test_df_plays_plot_title$defensiveTeam}),
       subtitle = glue({test_df_plays_plot_title$playDescription}),
       caption = "Chart & Model: @TAlbTree",
       fill = "Expected Tackle %") +
  theme(plot.title = element_text(size = 8),
        plot.subtitle = element_text(size = 6),
        plot.caption = element_text(size = 8)) +
  transition_time(frameId) +
  ease_aes('linear')

animate(animation, width = 1000, height = 650, res = 150, fps = 10)
anim_save(glue("plots/{test_df_plays_plot_title$gameId}_{test_df_plays_plot_title$playId}_{test_df_plays_plot_title$ballCarrierDisplayName}.gif"), 
          dpi = 1000, width = 1000, height = 650)


