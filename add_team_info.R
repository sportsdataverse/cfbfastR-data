library(cfbfastR)
library(git2r)
library(dplyr)
library(arrow)
library(purrr)
library(glue)
library(readr)
league <- 'cfb'
folder <- 'team_info'

games <- purrr::map_dfr(2001:cfbfastR:::most_recent_season(), 
                        function(x){
                          games <- cfbfastR::cfbd_team_info(year = x,only_fbs = FALSE)
                          # write_csv(games, glue::glue("{folder}/csv/{folder}_{x}.csv"))
                          saveRDS(games, glue::glue("{folder}/rds/{league}_{folder}_{x}.rds"))
                          arrow::write_parquet(games,glue::glue("{folder}/parquet/{league}_{folder}_{x}.parquet"))
                          
                        })

