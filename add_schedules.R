library(cfbfastR)
library(dplyr)
library(arrow)
library(purrr)
library(glue)


games <- purrr::map_dfr(2001:cfbfastR:::most_recent_cfb_season(), function(x) {
  games <- tryCatch({
    cfbfastR::cfbd_game_info(x)
  }, error = function(e) {
    message <- sprintf("Error retrieving game info for season %s: %s", x, conditionMessage(e))
    stop(message)
  })
  2001:cfbfastR:::most_recent_cfb_season(), 
  function(x) {
    games <- cfbfastR::cfbd_game_info(x)
    readr::write_csv(games, glue::glue("schedules/csv/cfb_schedules_{x}.csv"))
    saveRDS(games, glue::glue("schedules/rds/cfb_schedules_{x}.rds"))
    arrow::write_parquet(games, glue::glue("schedules/parquet/cfb_schedules_{x}.parquet"))
    arrow::write_parquet(games, glue::glue("schedules/parquet/schedules_{x}.parquet"))
  }
