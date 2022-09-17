library(cfbfastR)
library(dplyr)
library(arrow)
library(purrr)
library(glue)


games <- purrr::map_dfr(
  2001:cfbfastR:::most_recent_cfb_season(), 
  function(x){
    games <- cfbfastR::cfbd_game_info(x)
    readr::write_csv(games, glue::glue("schedules/csv/cfb_schedules_{x}.csv"))
    saveRDS(games, glue::glue("schedules/rds/cfb_schedules_{x}.rds"))
    arrow::write_parquet(games,glue::glue("schedules/parquet/cfb_schedules_{x}.parquet"))
    arrow::write_parquet(games,glue::glue("schedules/parquet/schedules_{x}.parquet"))
  })


message <- sprintf("Updated %s (ET) using cfbfastR version %s", lubridate::now("America/New_York"), utils::packageVersion("cfbfastR"))

system(glue::glue('git config --local user.email "actions@GitHub.com" '))
system(glue::glue('git config --local user.name "GitHub Actions"'))
system(glue::glue('git add .'))
system(glue::glue('git commit -am "{message}"'))
system(glue::glue('git pull'))
system(glue::glue('git push'))
