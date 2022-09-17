library(cfbfastR)
library(dplyr)
library(arrow)
library(purrr)
library(glue)


games <- purrr::map(
  2004:cfbfastR:::most_recent_cfb_season(), 
  function(x){
    games <- cfbfastR::cfbd_team_roster(year=x)
    games$season <- x
    games$recruit_ids <- lapply(games$recruit_ids, function(y){
      if(length(y) == 0) as.integer(0) else y
    })
    readr::write_csv(games, glue::glue("rosters/csv/cfb_rosters_{x}.csv"))
    saveRDS(games, glue::glue("rosters/rds/cfb_rosters_{x}.rds"))
    arrow::write_parquet(games,glue::glue("rosters/parquet/cfb_rosters_{x}.parquet"))
    arrow::write_parquet(games,glue::glue("rosters/parquet/rosters_{x}.parquet"))
    
  })

