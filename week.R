library(cfbfastR)
library(dplyr)
library(tidyr)
library(readr)
library(purrr)
library(arrow)
library(glue)


# Play-by-Play Data Pull --------------------------------------------------
week_vector = 1:15
year_vector = 2021

current_season <- year_vector[length(year_vector)] # in case year_vector is actually a vector, grab last year
version = packageVersion("cfbfastR")
weekly_year_df = expand.grid(year = year_vector, week = week_vector)

### scrape yearly
year_split = split(weekly_year_df, weekly_year_df$year)

yr_epa_start_time <- proc.time()

for (i in 1:length(year_split)) {
  i=1
  print(paste0("Working on ", year_split[[i]][1,1]))
  year_split[[i]] = year_split[[i]] %>% 
    dplyr::mutate(
      pbp = purrr::map2(
        .x = year,
        .y = week,
        cfbfastR::cfbd_pbp_data,
        season_type = 'both',
        epa_wpa=TRUE
        
      ))
  Sys.sleep(1)
}

yr_epa_season_run <- proc.time() - yr_epa_start_time
print(yr_epa_season_run['elapsed']/60)
year_split20 = lapply(year_split, function(x) {
  x %>% tidyr::unnest(pbp, names_repair="minimal")
})

all_years_20 = dplyr::bind_rows(year_split20)
all_years_20 <- all_years_20 

# Update schedules ---------------------------------------------------------
schedules <- cfbd_game_info(year_vector[length(year_vector)])
schedules <- schedules %>% 
  cfbfastR:::make_cfbfastR_data("Games and schedules from data repository",Sys.time())

write.csv(schedules, glue::glue('schedules/csv/cfb_schedules_{current_season}.csv'), row.names = FALSE)
saveRDS(schedules,glue::glue('schedules/rds/cfb_schedules_{current_season}.rds'))
arrow::write_parquet(schedules,glue::glue('schedules/parquet/cfb_schedules_{current_season}.parquet'))

# Player Stats ------------------------------------------------------------

df_game_ids <- unique(all_years_20$game_id)
df_player_stats_2021<- data.frame()
for(i in 1:length(df_game_ids)){
  print(paste0("Working on ", i,"/",length(df_game_ids),": ", df_game_ids[i]))
  df_play_stats <- cfbfastR::cfbd_play_stats_player(game_id = df_game_ids[i])
  df_player_stats_2021 <- rbind(df_player_stats_2021, df_play_stats)
}

df_player_stats_2021 <- df_player_stats_2021 %>%
  dplyr::mutate(
    drive_id = as.numeric(drive_id),
    reception_player_id = as.integer(reception_player_id),
    target_player_id = as.integer(target_player_id),
    completion_player_id = as.integer(completion_player_id),
    incompletion_player_id = as.integer(incompletion_player_id),
    rush_player_id = as.integer(rush_player_id),
    touchdown_player_id = as.integer(touchdown_player_id),
    interception_player_id = as.integer(interception_player_id),
    interception_thrown_player_id = as.integer(interception_thrown_player_id),
    fumble_recovered_player_id = as.integer(fumble_recovered_player_id),
    fumble_forced_player_id = as.integer(fumble_forced_player_id),
    fumble_player_id = as.integer(fumble_player_id),
    sack_player_id = as.integer(sack_player_id),
    sack_taken_player_id = as.integer(sack_taken_player_id),
    pass_breakup_player_id = as.integer(pass_breakup_player_id))

saveRDS(df_player_stats_2021 %>% 
          dplyr::filter(.data$season == current_season), 
        glue::glue("player_stats/rds/player_stats_{current_season}.rds"))
readr::write_csv(df_player_stats_2021 %>% 
                   dplyr::filter(.data$season == current_season), 
                 glue::glue("player_stats/csv/player_stats_{current_season}.csv"))
arrow::write_parquet(df_player_stats_2021 %>% 
                       dplyr::filter(.data$season == current_season),
                     glue::glue('player_stats/parquet/player_stats_{current_season}.parquet'))


df_year_players20 <- all_years_20 %>% 
  dplyr::left_join(df_player_stats_2021, 
                   by = c('id_play'="play_id",'game_id','drive_id',
                          'period','down','distance','yards_to_goal','week','season'))


df_team_rosters_2021 <- read.csv(glue::glue('rosters/csv/cfb_rosters_{current_season}.csv'))



df_year_players_pos20 <- df_year_players20 %>% 
  dplyr::left_join(df_team_rosters_2021 %>% 
                     dplyr::select(season, athlete_id, position) %>% 
                     dplyr::mutate(athlete_id = as.numeric(athlete_id)),
                   by = c("year"="season", "reception_player_id" = "athlete_id")) %>% 
  dplyr::left_join(df_team_rosters_2021 %>% 
                     dplyr::select(season, athlete_id, position) %>% 
                     dplyr::mutate(athlete_id = as.numeric(athlete_id)),
                   by = c("year"="season", "target_player_id" = "athlete_id"), suffix=c("_reception","_target")) %>% 
  dplyr::left_join(df_team_rosters_2021 %>% 
                     dplyr::select(season, athlete_id, position) %>% 
                     dplyr::mutate(athlete_id = as.numeric(athlete_id)) %>% 
                     dplyr::rename(position_completion = position),
                   by = c("year"="season", "completion_player_id" = "athlete_id")) %>% 
  dplyr::left_join(df_team_rosters_2021 %>% 
                     dplyr::select(season, athlete_id, position) %>% 
                     dplyr::mutate(athlete_id = as.numeric(athlete_id)) %>% 
                     dplyr::rename(position_incompletion = position),
                   by = c("year"="season", "incompletion_player_id" = "athlete_id")) %>% 
  dplyr::left_join(df_team_rosters_2021 %>% 
                     dplyr::select(season, athlete_id, position) %>% 
                     dplyr::mutate(athlete_id = as.numeric(athlete_id)) %>% 
                     dplyr::rename(position_sack_taken = position),
                   by = c("year"="season", "sack_taken_player_id" = "athlete_id")) %>% 
  dplyr::left_join(df_team_rosters_2021 %>% 
                     dplyr::select(season, athlete_id, position) %>% 
                     dplyr::mutate(athlete_id = as.numeric(athlete_id)) %>% 
                     dplyr::rename(position_sack = position),
                   by = c("year"="season", "sack_player_id" = "athlete_id")) %>% 
  dplyr::left_join(df_team_rosters_2021 %>% 
                     dplyr::select(season, athlete_id, position) %>% 
                     dplyr::mutate(athlete_id = as.numeric(athlete_id)) %>% 
                     dplyr::rename(position_interception_thrown = position),
                   by = c("year"="season", "interception_thrown_player_id" = "athlete_id")) %>% 
  dplyr::left_join(df_team_rosters_2021 %>% 
                     dplyr::select(season, athlete_id, position) %>% 
                     dplyr::mutate(athlete_id = as.numeric(athlete_id)) %>% 
                     dplyr::rename(position_interception = position),
                   by = c("year"="season", "interception_player_id" = "athlete_id")) %>% 
  dplyr::left_join(df_team_rosters_2021 %>% 
                     dplyr::select(season, athlete_id, position) %>% 
                     dplyr::mutate(athlete_id = as.numeric(athlete_id)) %>% 
                     dplyr::rename(position_fumble = position),
                   by = c("year"="season", "fumble_player_id" = "athlete_id")) %>% 
  dplyr::left_join(df_team_rosters_2021 %>% 
                     dplyr::select(season, athlete_id, position) %>% 
                     dplyr::mutate(athlete_id = as.numeric(athlete_id)) %>% 
                     dplyr::rename(position_fumble_forced = position),
                   by = c("year"="season", "fumble_forced_player_id" = "athlete_id")) %>% 
  dplyr::left_join(df_team_rosters_2021 %>% 
                     dplyr::select(season, athlete_id, position) %>% 
                     dplyr::mutate(athlete_id = as.numeric(athlete_id)) %>% 
                     dplyr::rename(position_fumble_recovered = position),
                   by = c("year"="season", "fumble_recovered_player_id" = "athlete_id")) %>% 
  dplyr::left_join(df_team_rosters_2021 %>% 
                     dplyr::select(season, athlete_id, position) %>% 
                     dplyr::mutate(athlete_id = as.numeric(athlete_id)) %>% 
                     dplyr::rename(position_pass_breakup = position),
                   by = c("year"="season", "pass_breakup_player_id" = "athlete_id")) %>% 
  dplyr::left_join(df_team_rosters_2021 %>% 
                     dplyr::select(season, athlete_id, position) %>% 
                     dplyr::mutate(athlete_id = as.numeric(athlete_id)) %>% 
                     dplyr::rename(position_rush = position),
                   by = c("year"="season", "rush_player_id" = "athlete_id")) %>% 
  dplyr::left_join(df_team_rosters_2021 %>% 
                     dplyr::select(season, athlete_id, position) %>% 
                     dplyr::mutate(athlete_id = as.numeric(athlete_id)) %>% 
                     dplyr::rename(position_touchdown = position),
                   by = c("year"="season", "touchdown_player_id" = "athlete_id")) 

df_year_players_pos20 <- df_year_players_pos20 %>% 
  dplyr::mutate(position_target = ifelse(!is.na(position_reception), 
                                  position_reception, position_target)) %>% 
  as.data.frame()




play_columns = c(
  "year", "week", "id_play", "game_id", "game_play_number", "half_play_number", "drive_play_number", 
  "pos_team", "def_pos_team","pos_team_score", "def_pos_team_score",
  "half", "period", "clock.minutes", "clock.seconds", 
  "play_type", "play_text",
  "down", "distance", "yards_to_goal", "yards_gained" 
)
model_columns = c(
  "EPA", "ep_before", "ep_after", 
  "wpa", "wp_before", "wp_after",
  "def_wp_before", "def_wp_after",
  "penalty_detail", "yds_penalty","penalty_1st_conv"
)
series_columns = c(
  "new_series", "firstD_by_kickoff", "firstD_by_poss", "firstD_by_penalty", "firstD_by_yards"
)
epa_flag_columns = c( 
  "def_EPA", "home_EPA", "away_EPA", 
  "home_EPA_rush", "away_EPA_rush", 
  "home_EPA_pass", "away_EPA_pass", 
  "total_home_EPA", "total_away_EPA", 
  "total_home_EPA_rush", "total_away_EPA_rush", 
  "total_home_EPA_pass", "total_away_EPA_pass", 
  "net_home_EPA", "net_away_EPA", 
  "net_home_EPA_rush", "net_away_EPA_rush", 
  "net_home_EPA_pass", "net_away_EPA_pass",  
  "success", "epa_success", 
  "rz_play", "scoring_opp", 
  "middle_8", "stuffed_run"
  
)
team_columns = c(
  "change_of_pos_team", "downs_turnover", "turnover",
  "pos_score_diff_start", "pos_score_pts",  "log_ydstogo",
  "ExpScoreDiff", "ExpScoreDiff_Time_Ratio", "half_clock.minutes", 
  "TimeSecsRem", "adj_TimeSecsRem",  "Goal_To_Go", "Under_two", 
  "home", "away", "home_wp_before", "away_wp_before", "home_wp_after", "away_wp_after", 
  "end_of_half", "pos_team_receives_2H_kickoff",
  "lead_pos_team",  "lead_play_type","lag_pos_team", "lag_play_type",
  "orig_play_type",  "Under_three"
)

model_end_columns = c(
  "down_end", "distance_end", "log_ydstogo_end", "yards_to_goal_end", 
  "TimeSecsRem_end", "Goal_To_Go_end", "Under_two_end",    
  "offense_score_play", "defense_score_play",
  "ppa", "yard_line", "scoring",
  "pos_team_timeouts_rem_before", "def_pos_team_timeouts_rem_before",
  "pos_team_timeouts", "def_pos_team_timeouts", 
  "pos_score_diff", "pos_score_diff_start_end", 
  "offense_play", "defense_play", 
  "offense_receives_2H_kickoff", "change_of_poss", 
  "score_pts", "score_diff_start", "score_diff", 
  "offense_score", "defense_score",
  "offense_conference", "defense_conference",
  "off_timeout_called", "def_timeout_called", 
  "offense_timeouts", "defense_timeouts", 
  "off_timeouts_rem_before", "def_timeouts_rem_before" 
) 
player_name_columns = c(             
  "rusher_player_name", "yds_rushed", "passer_player_name", "receiver_player_name", "yds_receiving", 
  "yds_sacked", "sack_players","sack_player_name", "sack_player_name2", 
  "pass_breakup_player_name", "interception_player_name", "yds_int_return", 
  "fumble_player_name", "fumble_forced_player_name", "fumble_recovered_player_name", "yds_fumble_return", 
  "punter_player_name", "yds_punted", "punt_returner_player_name", "yds_punt_return", "yds_punt_gained", 
  "punt_block_player_name", "punt_block_return_player_name", 
  "fg_kicker_player_name", "yds_fg", "fg_block_player_name", "fg_return_player_name", 
  "kickoff_player_name", "yds_kickoff", "kickoff_returner_player_name", "yds_kickoff_return", "new_id"
)             
drive_columns = c(
  "orig_drive_number", "drive_number", 
  "drive_result_detailed", "new_drive_pts", "drive_id", "drive_result",
  "drive_start_yards_to_goal", "drive_end_yards_to_goal", "drive_yards", "drive_scoring", "drive_pts", 
  "drive_start_period", "drive_end_period", "drive_time_minutes_start", 
  "drive_time_seconds_start", "drive_time_minutes_end", "drive_time_seconds_end", 
  "drive_time_minutes_elapsed", "drive_time_seconds_elapsed", 
  "drive_numbers", "number_of_drives", "pts_scored", "drive_result_detailed_flag", "drive_result2", 
  "drive_num", "lag_drive_result_detailed", "lead_drive_result_detailed", 
  "lag_new_drive_pts", "id_drive"
)
penalty_columns = c(
  "penalty_flag", "penalty_declined", 
  "penalty_no_play", "penalty_offset",  
  "penalty_text", "penalty_play_text"
) 
play_flag_columns = c(
  "rush", "rush_td", "pass", "pass_td",  
  "completion", "pass_attempt", "target", 
  "sack_vec", "sack", "int", "int_td",  
  "turnover_vec", "turnover_vec_lag", "turnover_indicator", 
  "kickoff_play", "receives_2H_kickoff", "missing_yard_flag",  
  "scoring_play", "td_play", "touchdown", "safety", "fumble_vec", 
  "kickoff_tb", "kickoff_onside", "kickoff_oob", "kickoff_fair_catch", "kickoff_downed", 
  "kickoff_safety", "kick_play", 
  "punt", "punt_play", "punt_tb", "punt_oob", "punt_fair_catch", "punt_downed", 
  "punt_safety",  "punt_blocked", "penalty_safety",
  "fg_inds", "fg_made", "fg_make_prob"
)  
model_prob_columns = c(
  "No_Score_before", "FG_before", "Opp_FG_before", "Opp_Safety_before", 
  "Opp_TD_before", "Safety_before", "TD_before", 
  "No_Score_after", "FG_after", "Opp_FG_after", "Opp_Safety_after",
  "Opp_TD_after", "Safety_after", "TD_after"
)
wpa_extra_columns = c(   
  "lead_wp_before2", "wpa_half_end",   "wpa_base", "wpa_base_nxt", "wpa_change", "wpa_change_nxt", 
  "wpa_base_ind", "wpa_base_nxt_ind", "wpa_change_ind", "wpa_change_nxt_ind", "lead_wp_before", 
  "lead_pos_team2"
)
lag_series_columns = c(
  "row", "drive_event_number",
  "orig_play_type", "lead_play_type",
  "lag_play_type","lag_play_type2","lag_play_type3",
  "lag_play_text","lag_play_text2","lead_play_text",
  "lag_first_by_penalty", "lag_first_by_penalty2",
  "lag_first_by_yards","lag_first_by_yards2", 
  "first_by_penalty", "first_by_yards", "play_after_turnover",
  "lag_change_of_poss","lag_change_of_pos_team", "lag_change_of_pos_team2", 
  "lag_kickoff_play", "lag_punt", "lag_punt2", 
  "lag_scoring_play", "lag_turnover_vec", 
  "lag_downs_turnover", "lag_defense_score_play"
)    
lag_lead_columns = c(
  "lag_score_diff", "lag_offense_play", "lead_offense_play", "lead_offense_play2", 
  "lag_pos_score_diff","lag_off_timeouts",
  "lag_def_timeouts", "lag_TimeSecsRem2", "lag_TimeSecsRem", "lead_TimeSecsRem", 
  "lead_TimeSecsRem2", "lag_yards_to_goal2", "lag_yards_to_goal", 
  "lead_yards_to_goal", "lead_yards_to_goal2", "lag_down2", "lag_down", 
  "lead_down", "lead_down2", "lead_distance", "lead_distance2", "lead_play_type2", "lead_play_type3", 
  # "lag_change_of_poss","lag_change_of_pos_team", "lag_kickoff_play", "lag_punt", "lag_scoring_play", 
  # "lag_turnover_vec", "lag_downs_turnover", "lag_defense_score_play",  
  "lag_ep_before3", "lag_ep_before2", "lag_ep_before", "lead_ep_before", "lead_ep_before2", 
  "lag_ep_after", "lag_ep_after2", "lag_ep_after3", "lead_ep_after", "lead_ep_after2" 
)


play_stats_player_columns = c(
  "position_reception", "position_target", 
  "position_completion", "position_incompletion", "position_sack_taken", 
  "position_sack", "position_interception_thrown", "position_interception", 
  "position_fumble", "position_fumble_forced", "position_fumble_recovered", 
  "position_pass_breakup", "position_rush", "position_touchdown", "new_id",
  "season", "opponent", "team_score", "opponent_score",  
  "rush_player_id", "rush_player", "rush_yds",
  "reception_player_id", "reception_player", "reception_yds", "completion_player_id", 
  "completion_player", "completion_yds","interception_player_id", "interception_player", 
  "interception_stat", "interception_thrown_player_id", "interception_thrown_player", 
  "interception_thrown_stat", "touchdown_player_id", "touchdown_player", 
  "touchdown_stat", "incompletion_player_id", "incompletion_player", "incompletion_stat", 
  "target_player_id", "target_player", "target_stat", 
  "fumble_recovered_player_id", "fumble_recovered_player", "fumble_recovered_stat", 
  "fumble_forced_player_id", "fumble_forced_player", "fumble_forced_stat", 
  "fumble_player_id", "fumble_player", "fumble_stat", "sack_player_id", 
  "sack_player", "sack_stat", "sack_taken_player_id", "sack_taken_player", 
  "sack_taken_stat", "pass_breakup_player_id", "pass_breakup_player", 
  "pass_breakup_stat"
)



df_year_players_pos20 <- df_year_players_pos20 %>% 
  dplyr::select(
    dplyr::all_of(play_columns),
    dplyr::all_of(model_columns),
    dplyr::all_of(series_columns),
    dplyr::all_of(epa_flag_columns),
    dplyr::all_of(team_columns),
    dplyr::all_of(model_end_columns),
    dplyr::all_of(player_name_columns),
    dplyr::all_of(drive_columns),
    dplyr::all_of(play_flag_columns),
    dplyr::all_of(model_prob_columns),
    dplyr::all_of(play_stats_player_columns),
    dplyr::all_of(penalty_columns),
    dplyr::all_of(lag_series_columns)
  ) %>% dplyr::mutate(season = .data$year)

# Update games in data repo file ------------------------------------------

game_ids <- readRDS('data/games_in_data_repo.rds')
df_game_ids <- dplyr::bind_rows(
  as.data.frame(dplyr::distinct(df_year_players_pos20 %>% 
                                  dplyr::select(game_id, year, week, home, away))), game_ids) %>% 
  dplyr::distinct(game_id, year, week, home, away) %>% 
  as.data.frame() %>% 
  dplyr::arrange(-year, -week, home, away, game_id)
df_game_ids <- df_game_ids %>% dplyr::mutate(season=.data$year)
df_year_players_pos20 <- df_year_players_pos20 %>% 
  dplyr::mutate_at(c("id_play","half","down_end","ppa","id_drive"), as.numeric)
write.csv(df_game_ids, 'data/games_in_data_repo.csv', row.names = FALSE)
saveRDS(df_game_ids, 'data/games_in_data_repo.rds')
saveRDS(df_year_players_pos20,glue::glue('data/rds/pbp_players_pos_{current_season}.rds'))
arrow::write_parquet(df_year_players_pos20,glue::glue('data/parquet/pbp_players_pos_{current_season}.parquet'))

message <- sprintf("Updated %s (ET) using cfbfastR version %s", lubridate::now("America/New_York"), utils::packageVersion("cfbfastR"))

system(glue::glue('git config --local user.email "actions@GitHub.com" '))
system(glue::glue('git config --local user.name "GitHub Actions"'))
system(glue::glue('git add .'))
system(glue::glue('git commit -am "{message}"'))
system(glue::glue('git pull'))
system(glue::glue('git push'))
