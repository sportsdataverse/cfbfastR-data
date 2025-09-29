remotes::install_github("sportsdataverse/cfbfastR")
library(cfbfastR)
library(dplyr)
library(tidyr)
library(readr)
library(purrr)
library(progressr)
library(furrr)
library(future)
library(arrow)
library(glue)
library(optparse)


option_list <- list(
  make_option(
    c("-s", "--start_year"),
    action = "store",
    default = cfbfastR:::most_recent_cfb_season(),
    type = "integer",
    help = "Start year of the seasons to process"
  ),
  make_option(
    c("-e", "--end_year"),
    action = "store",
    default = cfbfastR:::most_recent_cfb_season(),
    type = "integer",
    help = "End year of the seasons to process"
  )
)
opt <- parse_args(OptionParser(option_list = option_list))
options(stringsAsFactors = FALSE)
options(scipen = 999)

# Play-by-Play Data Pull --------------------------------------------------
week_vector <- 1:15
year_vector <- 2025

current_season <- year_vector[length(year_vector)] # in case year_vector is actually a vector, grab last year
version <- packageVersion("cfbfastR")
weekly_year_df <- expand.grid(year = year_vector, week = week_vector)

### scrape yearly
week_year_split <- split(weekly_year_df, weekly_year_df$year)
x <- week_year_split[[1]]$year
y <- week_year_split[[1]]$week
yr_epa_start_time <- proc.time()

if (interactive()) {
  future::plan("multisession", workers = 8)
  progressr::with_progress({
    p <- progressr::progressor(along = y)
    pbp_df <- furrr::future_map2_dfr(
      .x = x,
      .y = y,
      function(.x, .y) {
        Sys.sleep(1)
        pbp <- cfbfastR::cfbd_pbp_data(
          year = .x,
          week = .y,
          season_type = "both",
          epa_wpa = TRUE
        )
        p()
        return(pbp)
      }
    )
  })
  future::plan("sequential")
} else {
  # Non-interactive version
  pbp_df <- purrr::map2(
    .x = x,
    .y = y,
    function(.x, .y) {
      cfbfastR::cfbd_pbp_data(
        year = .x,
        week = .y,
        season_type = "both",
        epa_wpa = TRUE
      )
    },
    .progress = TRUE
  ) %>%
    purrr::list_rbind()
}


yr_epa_season_run <- proc.time() - yr_epa_start_time
print(yr_epa_season_run["elapsed"] / 60)

# Update schedules ---------------------------------------------------------
schedules <- cfbd_game_info(year_vector[length(year_vector)])
schedules <- schedules %>%
  cfbfastR:::make_cfbfastR_data(
    "Games and schedules from data repository",
    Sys.time()
  )

pbp_df <- pbp_df %>%
  dplyr::left_join(
    schedules %>%
      dplyr::select(dplyr::any_of(c(
        "game_id",
        "season",
        "week",
        "venue_id",
        "venue",
        "neutral_site",
        "conference_game",
        "season_type",
        "start_date",
        "completed",
        "home_team_id" = "home_id",
        "home_team",
        "home_team_division" = "home_division",
        "home_team_conference" = "home_conference",
        "home_team_pregame_elo" = "home_pregame_elo",
        "away_team_id" = "away_id",
        "away_team",
        "away_team_division" = "away_division",
        "away_team_conference" = "away_conference",
        "away_team_pregame_elo" = "away_pregame_elo"
      ))),
    by = c("game_id", "season", "wk" = "week"),
    suffix = c("", "_y")
  )

pbp_df <- pbp_df %>%
  dplyr::rename(dplyr::any_of(c(
    "year" = "season",
    "week" = "wk"
  ))) %>%
  dplyr::mutate(
    season = .data$year
  )

write.csv(
  schedules,
  glue::glue("schedules/csv/cfb_schedules_{current_season}.csv"),
  row.names = FALSE
)
saveRDS(
  schedules,
  glue::glue("schedules/rds/cfb_schedules_{current_season}.rds")
)
arrow::write_parquet(
  schedules,
  glue::glue("schedules/parquet/cfb_schedules_{current_season}.parquet")
)

# Player Stats ------------------------------------------------------------

df_game_ids <- unique(pbp_df$game_id)

if (interactive()) {
  future::plan("multisession", workers = 8)
  progressr::with_progress({
    p <- progressr::progressor(along = df_game_ids)
    df_player_stats <- furrr::future_map_dfr(
      .x = df_game_ids,
      function(.x) {
        player_stats <- cfbfastR::cfbd_play_stats_player(
          game_id = .x
        )
        p()
        return(player_stats)
      }
    )
  })
  future::plan("sequential")
} else {
  # Non-interactive version
  df_player_stats <- purrr::map(
    .x = df_game_ids,
    function(.x) {
      cfbfastR::cfbd_play_stats_player(
        game_id = .x
      )
    },
    .progress = TRUE
  ) %>%
    purrr::list_rbind()
}

player_stats_df <- df_player_stats

df_player_stats <- df_player_stats %>%
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
    pass_breakup_player_id = as.integer(pass_breakup_player_id)
  ) %>%
  dplyr::select(
    -dplyr::any_of(c(
      "athlete_id",
      "stat",
      "completion",
      "reception",
      "rush",
      "incompletion",
      "target",
      "field_goal_attempt",
      "field_goal_made",
      "sack_taken",
      "touchdown",
      "sack",
      "interception",
      "interception_thrown",
      "fumble_recovered",
      "fumble_forced",
      "fumble",
      "pass_breakup",
      "field_goal_missed",
      "fg_attempt_blocked"
    ))
  )


saveRDS(
  df_player_stats %>%
    dplyr::filter(.data$season == current_season),
  glue::glue("player_stats/rds/player_stats_{current_season}.rds")
)
readr::write_csv(
  df_player_stats %>%
    dplyr::filter(.data$season == current_season),
  glue::glue("player_stats/csv/player_stats_{current_season}.csv")
)
arrow::write_parquet(
  df_player_stats %>%
    dplyr::filter(.data$season == current_season),
  glue::glue("player_stats/parquet/player_stats_{current_season}.parquet")
)


df_year_players <- pbp_df %>%
  dplyr::left_join(
    df_player_stats,
    by = c(
      "id_play" = "play_id",
      "game_id",
      "drive_id",
      "period",
      "clock_minutes",
      "clock_seconds",
      "down",
      "distance",
      "yards_to_goal",
      "week",
      "season"
    )
  )


df_team_rosters <- read.csv(glue::glue(
  "rosters/csv/cfb_rosters_{current_season}.csv"
))


df_year_players_pos <- df_year_players %>%
  dplyr::left_join(
    df_team_rosters %>%
      dplyr::select(season, athlete_id, position) %>%
      dplyr::mutate(athlete_id = as.numeric(athlete_id)),
    by = c("year" = "season", "reception_player_id" = "athlete_id"),
    relationship = "many-to-many"
  ) %>%
  dplyr::left_join(
    df_team_rosters %>%
      dplyr::select(season, athlete_id, position) %>%
      dplyr::mutate(athlete_id = as.numeric(athlete_id)),
    by = c("year" = "season", "target_player_id" = "athlete_id"),
    suffix = c("_reception", "_target"),
    relationship = "many-to-many"
  ) %>%
  dplyr::left_join(
    df_team_rosters %>%
      dplyr::select(season, athlete_id, position) %>%
      dplyr::mutate(athlete_id = as.numeric(athlete_id)) %>%
      dplyr::rename(position_completion = position),
    by = c("year" = "season", "completion_player_id" = "athlete_id"),
    relationship = "many-to-many"
  ) %>%
  dplyr::left_join(
    df_team_rosters %>%
      dplyr::select(season, athlete_id, position) %>%
      dplyr::mutate(athlete_id = as.numeric(athlete_id)) %>%
      dplyr::rename(position_incompletion = position),
    by = c("year" = "season", "incompletion_player_id" = "athlete_id"),
    relationship = "many-to-many"
  ) %>%
  dplyr::left_join(
    df_team_rosters %>%
      dplyr::select(season, athlete_id, position) %>%
      dplyr::mutate(athlete_id = as.numeric(athlete_id)) %>%
      dplyr::rename(position_sack_taken = position),
    by = c("year" = "season", "sack_taken_player_id" = "athlete_id"),
    relationship = "many-to-many"
  ) %>%
  dplyr::left_join(
    df_team_rosters %>%
      dplyr::select(season, athlete_id, position) %>%
      dplyr::mutate(athlete_id = as.numeric(athlete_id)) %>%
      dplyr::rename(position_sack = position),
    by = c("year" = "season", "sack_player_id" = "athlete_id"),
    relationship = "many-to-many"
  ) %>%
  dplyr::left_join(
    df_team_rosters %>%
      dplyr::select(season, athlete_id, position) %>%
      dplyr::mutate(athlete_id = as.numeric(athlete_id)) %>%
      dplyr::rename(position_interception_thrown = position),
    by = c("year" = "season", "interception_thrown_player_id" = "athlete_id"),
    relationship = "many-to-many"
  ) %>%
  dplyr::left_join(
    df_team_rosters %>%
      dplyr::select(season, athlete_id, position) %>%
      dplyr::mutate(athlete_id = as.numeric(athlete_id)) %>%
      dplyr::rename(position_interception = position),
    by = c("year" = "season", "interception_player_id" = "athlete_id"),
    relationship = "many-to-many"
  ) %>%
  dplyr::left_join(
    df_team_rosters %>%
      dplyr::select(season, athlete_id, position) %>%
      dplyr::mutate(athlete_id = as.numeric(athlete_id)) %>%
      dplyr::rename(position_fumble = position),
    by = c("year" = "season", "fumble_player_id" = "athlete_id"),
    relationship = "many-to-many"
  ) %>%
  dplyr::left_join(
    df_team_rosters %>%
      dplyr::select(season, athlete_id, position) %>%
      dplyr::mutate(athlete_id = as.numeric(athlete_id)) %>%
      dplyr::rename(position_fumble_forced = position),
    by = c("year" = "season", "fumble_forced_player_id" = "athlete_id"),
    relationship = "many-to-many"
  ) %>%
  dplyr::left_join(
    df_team_rosters %>%
      dplyr::select(season, athlete_id, position) %>%
      dplyr::mutate(athlete_id = as.numeric(athlete_id)) %>%
      dplyr::rename(position_fumble_recovered = position),
    by = c("year" = "season", "fumble_recovered_player_id" = "athlete_id"),
    relationship = "many-to-many"
  ) %>%
  dplyr::left_join(
    df_team_rosters %>%
      dplyr::select(season, athlete_id, position) %>%
      dplyr::mutate(athlete_id = as.numeric(athlete_id)) %>%
      dplyr::rename(position_pass_breakup = position),
    by = c("year" = "season", "pass_breakup_player_id" = "athlete_id"),
    relationship = "many-to-many"
  ) %>%
  dplyr::left_join(
    df_team_rosters %>%
      dplyr::select(season, athlete_id, position) %>%
      dplyr::mutate(athlete_id = as.numeric(athlete_id)) %>%
      dplyr::rename(position_rush = position),
    by = c("year" = "season", "rush_player_id" = "athlete_id"),
    relationship = "many-to-many"
  ) %>%
  dplyr::left_join(
    df_team_rosters %>%
      dplyr::select(season, athlete_id, position) %>%
      dplyr::mutate(athlete_id = as.numeric(athlete_id)) %>%
      dplyr::rename(position_touchdown = position),
    by = c("year" = "season", "touchdown_player_id" = "athlete_id"),
    relationship = "many-to-many"
  )

df_year_players_pos <- df_year_players_pos %>%
  dplyr::mutate(
    position_target = ifelse(
      !is.na(position_reception),
      position_reception,
      position_target
    )
  ) %>%
  as.data.frame()


play_columns <- c(
  "year",
  "week",
  "id_play",
  "game_id",
  "game_play_number",
  "half_play_number",
  "drive_play_number",
  "pos_team",
  "def_pos_team",
  "pos_team_score",
  "def_pos_team_score",
  "half",
  "period",
  "clock_minutes",
  "clock_seconds",
  "play_type",
  "play_text",
  "down",
  "distance",
  "yards_to_goal",
  "yards_gained"
)
model_columns <- c(
  "EPA",
  "ep_before",
  "ep_after",
  "wpa",
  "wp_before",
  "wp_after",
  "def_wp_before",
  "def_wp_after",
  "penalty_detail",
  "yds_penalty",
  "penalty_1st_conv"
)
series_columns <- c(
  "new_series",
  "firstD_by_kickoff",
  "firstD_by_poss",
  "firstD_by_penalty",
  "firstD_by_yards"
)
epa_flag_columns <- c(
  "def_EPA",
  "home_EPA",
  "away_EPA",
  "home_EPA_rush",
  "away_EPA_rush",
  "home_EPA_pass",
  "away_EPA_pass",
  "total_home_EPA",
  "total_away_EPA",
  "total_home_EPA_rush",
  "total_away_EPA_rush",
  "total_home_EPA_pass",
  "total_away_EPA_pass",
  "net_home_EPA",
  "net_away_EPA",
  "net_home_EPA_rush",
  "net_away_EPA_rush",
  "net_home_EPA_pass",
  "net_away_EPA_pass",
  "success",
  "epa_success",
  "rz_play",
  "scoring_opp",
  "middle_8",
  "stuffed_run"
)
team_columns <- c(
  "change_of_pos_team",
  "downs_turnover",
  "turnover",
  "pos_score_diff_start",
  "pos_score_pts",
  "log_ydstogo",
  "ExpScoreDiff",
  "ExpScoreDiff_Time_Ratio",
  "half_clock_minutes",
  "TimeSecsRem",
  "adj_TimeSecsRem",
  "Goal_To_Go",
  "Under_two",
  "home",
  "away",
  "home_wp_before",
  "away_wp_before",
  "home_wp_after",
  "away_wp_after",
  "end_of_half",
  "pos_team_receives_2H_kickoff",
  "lead_pos_team",
  "lead_play_type",
  "lag_pos_team",
  "lag_play_type",
  "orig_play_type",
  "Under_three"
)

model_end_columns <- c(
  "down_end",
  "distance_end",
  "log_ydstogo_end",
  "yards_to_goal_end",
  "TimeSecsRem_end",
  "Goal_To_Go_end",
  "Under_two_end",
  "offense_score_play",
  "defense_score_play",
  "ppa",
  "yard_line",
  "scoring",
  "pos_team_timeouts_rem_before",
  "def_pos_team_timeouts_rem_before",
  "pos_team_timeouts",
  "def_pos_team_timeouts",
  "pos_score_diff",
  "pos_score_diff_start_end",
  "offense_play",
  "defense_play",
  "offense_receives_2H_kickoff",
  "change_of_poss",
  "score_pts",
  "score_diff_start",
  "score_diff",
  "offense_score",
  "defense_score",
  "offense_conference",
  "defense_conference",
  "off_timeout_called",
  "def_timeout_called",
  "offense_timeouts",
  "defense_timeouts",
  "off_timeouts_rem_before",
  "def_timeouts_rem_before"
)
player_name_columns <- c(
  "rusher_player_name",
  "yds_rushed",
  "passer_player_name",
  "receiver_player_name",
  "yds_receiving",
  "yds_sacked",
  "sack_players",
  "sack_player_name",
  "sack_player_name2",
  "pass_breakup_player_name",
  "interception_player_name",
  "yds_int_return",
  "fumble_player_name",
  "fumble_forced_player_name",
  "fumble_recovered_player_name",
  "yds_fumble_return",
  "punter_player_name",
  "yds_punted",
  "punt_returner_player_name",
  "yds_punt_return",
  "yds_punt_gained",
  "punt_block_player_name",
  "punt_block_return_player_name",
  "fg_kicker_player_name",
  "yds_fg",
  "fg_block_player_name",
  "fg_return_player_name",
  "kickoff_player_name",
  "yds_kickoff",
  "kickoff_returner_player_name",
  "yds_kickoff_return",
  "new_id"
)
drive_columns <- c(
  "orig_drive_number",
  "drive_number",
  "drive_result_detailed",
  "new_drive_pts",
  "drive_id",
  "drive_result",
  "drive_start_yards_to_goal",
  "drive_end_yards_to_goal",
  "drive_yards",
  "drive_scoring",
  "drive_pts",
  "drive_start_period",
  "drive_end_period",
  "drive_time_minutes_start",
  "drive_time_seconds_start",
  "drive_time_minutes_end",
  "drive_time_seconds_end",
  "drive_time_minutes_elapsed",
  "drive_time_seconds_elapsed",
  "drive_numbers",
  "number_of_drives",
  "pts_scored",
  "drive_result_detailed_flag",
  "drive_result2",
  "drive_num",
  "lag_drive_result_detailed",
  "lead_drive_result_detailed",
  "lag_new_drive_pts",
  "id_drive"
)
penalty_columns <- c(
  "penalty_flag",
  "penalty_declined",
  "penalty_no_play",
  "penalty_offset",
  "penalty_text",
  "penalty_play_text"
)
play_flag_columns <- c(
  "rush",
  "rush_td",
  "pass",
  "pass_td",
  "completion",
  "pass_attempt",
  "target",
  "sack_vec",
  "sack",
  "int",
  "int_td",
  "turnover_vec",
  "turnover_vec_lag",
  "turnover_indicator",
  "kickoff_play",
  "receives_2H_kickoff",
  "missing_yard_flag",
  "scoring_play",
  "td_play",
  "touchdown",
  "safety",
  "fumble_vec",
  "kickoff_tb",
  "kickoff_onside",
  "kickoff_oob",
  "kickoff_fair_catch",
  "kickoff_downed",
  "kickoff_safety",
  "kick_play",
  "punt",
  "punt_play",
  "punt_tb",
  "punt_oob",
  "punt_fair_catch",
  "punt_downed",
  "punt_safety",
  "punt_blocked",
  "penalty_safety",
  "fg_inds",
  "fg_made",
  "fg_make_prob"
)
model_prob_columns <- c(
  "No_Score_before",
  "FG_before",
  "Opp_FG_before",
  "Opp_Safety_before",
  "Opp_TD_before",
  "Safety_before",
  "TD_before",
  "No_Score_after",
  "FG_after",
  "Opp_FG_after",
  "Opp_Safety_after",
  "Opp_TD_after",
  "Safety_after",
  "TD_after"
)
wpa_extra_columns <- c(
  "lead_wp_before2",
  "wpa_half_end",
  "wpa_base",
  "wpa_base_nxt",
  "wpa_change",
  "wpa_change_nxt",
  "wpa_base_ind",
  "wpa_base_nxt_ind",
  "wpa_change_ind",
  "wpa_change_nxt_ind",
  "lead_wp_before",
  "lead_pos_team2"
)
game_drive_columns <- c(
  "row",
  "drive_event_number",
  "orig_play_type",
  "lead_play_type",
  "play_number",
  "wallclock",
  "provider",
  "spread",
  "formatted_spread",
  "over_under",
  "drive_is_home_offense",
  "drive_start_offense_score",
  "drive_start_defense_score",
  "drive_end_offense_score",
  "drive_end_defense_score",
  "play",
  "event",
  "game_event_number",
  "game_row_number",
  "half_play",
  "half_event",
  "half_event_number",
  "half_row_number",
  "pos_unit",
  "def_pos_unit",
  "drive_play",
  "drive_event",
  "venue_id",
  "venue",
  "neutral_site",
  "conference_game",
  "season_type",
  "start_date",
  "completed",
  "home_team_id",
  "home_team",
  "home_team_division",
  "home_team_conference",
  "home_team_pregame_elo",
  "away_team_id",
  "away_team",
  "away_team_division",
  "away_team_conference",
  "away_team_pregame_elo",
  "season",
  "team",
  "conference",
  "opponent",
  "team_score",
  "opponent_score"
)
lag_series_columns <- c(
  "row",
  "drive_event_number",
  "orig_play_type",
  "lead_play_type",
  "lag_play_type",
  "lag_play_type2",
  "lag_play_type3",
  "lag_play_text",
  "lag_play_text2",
  "lead_play_text",
  "lag_first_by_penalty",
  "lag_first_by_penalty2",
  "lag_first_by_yards",
  "lag_first_by_yards2",
  "first_by_penalty",
  "first_by_yards",
  "play_after_turnover",
  "lag_change_of_poss",
  "lag_change_of_pos_team",
  "lag_change_of_pos_team2",
  "lag_kickoff_play",
  "lag_punt",
  "lag_punt2",
  "lag_scoring_play",
  "lag_turnover_vec",
  "lag_downs_turnover",
  "lag_defense_score_play"
)
lag_lead_columns <- c(
  "lag_score_diff",
  "lag_offense_play",
  "lead_offense_play",
  "lead_offense_play2",
  "lag_pos_score_diff",
  "lag_off_timeouts",
  "lag_def_timeouts",
  "lag_TimeSecsRem2",
  "lag_TimeSecsRem",
  "lead_TimeSecsRem",
  "lead_TimeSecsRem2",
  "lag_yards_to_goal2",
  "lag_yards_to_goal",
  "lead_yards_to_goal",
  "lead_yards_to_goal2",
  "lag_down2",
  "lag_down",
  "lead_down",
  "lead_down2",
  "lead_distance",
  "lead_distance2",
  "lead_play_type2",
  "lead_play_type3",
  # "lag_change_of_poss","lag_change_of_pos_team", "lag_kickoff_play", "lag_punt", "lag_scoring_play",
  # "lag_turnover_vec", "lag_downs_turnover", "lag_defense_score_play",
  "lag_ep_before3",
  "lag_ep_before2",
  "lag_ep_before",
  "lead_ep_before",
  "lead_ep_before2",
  "lag_ep_after",
  "lag_ep_after2",
  "lag_ep_after3",
  "lead_ep_after",
  "lead_ep_after2"
)


play_stats_player_columns <- c(
  "position_reception",
  "position_target",
  "position_completion",
  "position_incompletion",
  "position_sack_taken",
  "position_sack",
  "position_interception_thrown",
  "position_interception",
  "position_fumble",
  "position_fumble_forced",
  "position_fumble_recovered",
  "position_pass_breakup",
  "position_rush",
  "position_touchdown",
  "new_id",
  "season",
  "opponent",
  "team_score",
  "opponent_score",
  "rush_player_id",
  "rush_player",
  "rush_yds",
  "reception_player_id",
  "reception_player",
  "reception_yds",
  "completion_player_id",
  "completion_player",
  "completion_yds",
  "interception_player_id",
  "interception_player",
  "interception_stat",
  "interception_thrown_player_id",
  "interception_thrown_player",
  "interception_thrown_stat",
  "touchdown_player_id",
  "touchdown_player",
  "touchdown_stat",
  "incompletion_player_id",
  "incompletion_player",
  "incompletion_stat",
  "target_player_id",
  "target_player",
  "target_stat",
  "fumble_recovered_player_id",
  "fumble_recovered_player",
  "fumble_recovered_stat",
  "fumble_forced_player_id",
  "fumble_forced_player",
  "fumble_forced_stat",
  "fumble_player_id",
  "fumble_player",
  "fumble_stat",
  "sack_player_id",
  "sack_player",
  "sack_stat",
  "sack_taken_player_id",
  "sack_taken_player",
  "sack_taken_stat",
  "pass_breakup_player_id",
  "pass_breakup_player",
  "pass_breakup_stat",
  "field_goal_attempt_player_id",
  "field_goal_attempt_player",
  "field_goal_attempt_stat",
  "field_goal_made_player_id",
  "field_goal_made_player",
  "field_goal_made_stat",
  "field_goal_missed_player_id",
  "field_goal_missed_player",
  "field_goal_missed_stat",
  "field_goal_blocked_player_id",
  "field_goal_blocked_player",
  "field_goal_blocked_stat"
)


df_year_players_pos <- df_year_players_pos %>%
  dplyr::select(
    dplyr::all_of(play_columns),
    dplyr::all_of(model_columns),
    dplyr::all_of(series_columns),
    dplyr::all_of(epa_flag_columns),
    dplyr::all_of(team_columns),
    dplyr::all_of(game_drive_columns),
    dplyr::all_of(model_end_columns),
    dplyr::all_of(player_name_columns),
    dplyr::all_of(drive_columns),
    dplyr::all_of(play_flag_columns),
    dplyr::all_of(model_prob_columns),
    dplyr::all_of(play_stats_player_columns),
    dplyr::all_of(penalty_columns),
    # dplyr::all_of(lag_series_columns)
  ) %>%
  dplyr::mutate(season = .data$year)

# Update games in data repo file ------------------------------------------

game_ids <- readRDS("data/games_in_data_repo.rds")
df_game_ids <- dplyr::bind_rows(
  as.data.frame(dplyr::distinct(
    df_year_players_pos %>%
      dplyr::select(game_id, year, week, home, away)
  )),
  game_ids
) %>%
  dplyr::distinct(game_id, year, week, home, away) %>%
  as.data.frame() %>%
  dplyr::arrange(-year, -week, home, away, game_id)
df_game_ids <- df_game_ids %>% dplyr::mutate(season = .data$year)
df_year_players_pos <- df_year_players_pos %>%
  dplyr::mutate_at(
    c("id_play", "half", "down_end", "ppa", "id_drive"),
    as.numeric
  ) %>%
  cfbfastR:::make_cfbfastR_data(
    "PBP from data repo and CollegeFootballData.com",
    Sys.time()
  )


write.csv(df_game_ids, "data/games_in_data_repo.csv", row.names = FALSE)
saveRDS(df_game_ids, "data/games_in_data_repo.rds")
saveRDS(
  df_year_players_pos,
  glue::glue("data/rds/pbp_players_pos_{current_season}.rds")
)
df_year_players_pos <- readRDS(glue::glue(
  "data/rds/pbp_players_pos_{current_season}.rds"
))
retry_rate <- purrr::rate_backoff(
  pause_base = 1,
  pause_min = 60,
  max_times = 10
)
purrr::insistently(
  sportsdataversedata::sportsdataverse_save,
  rate = retry_rate,
  quiet = FALSE
)(
  pkg_function = "cfbfastR::load_cfb_pbp()",
  data_frame = df_year_players_pos,
  file_name = glue::glue("play_by_play_{current_season}"),
  sportsdataverse_type = "play-by-play data",
  release_tag = "cfbfastR_cfb_pbp",
  file_types = c("rds", "csv", "parquet"),
  .token = Sys.getenv("GITHUB_PAT")
)
# sportsdataversedata::sportsdataverse_save(
#   pkg_function = "cfbfastR::load_cfb_pbp()",
#   data_frame = df_year_players_pos,
#   file_name =  glue::glue("play_by_play_{current_season}"),
#   sportsdataverse_type = "play-by-play data",
#   release_tag = "cfbfastR_cfb_pbp",
#   file_types = c("rds", "csv", "parquet"),
#   .token = Sys.getenv("GITHUB_PAT")
# )

message <- sprintf(
  "Updated %s (ET) using cfbfastR version %s",
  lubridate::now("America/New_York"),
  utils::packageVersion("cfbfastR")
)

system(glue::glue('git config --local user.email "actions@GitHub.com" '))
system(glue::glue('git config --local user.name "GitHub Actions"'))
system(glue::glue("git add ."))
system(glue::glue('git commit -am "{message}"'))
system(glue::glue("git pull"))
system(glue::glue("git push"))
