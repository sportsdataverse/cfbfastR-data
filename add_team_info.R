library(cfbfastR)
library(git2r)
library(dplyr)
library(arrow)
library(purrr)
library(glue)
library(readr)

folder <- 'team_info'
repo <- git2r::repository('./') 
games <- purrr::map_dfr(2001:2020, 
                        function(x){
                          games <- cfbfastR::cfbd_team_info(year = x,only_fbs = FALSE)
                          # write_csv(games, glue::glue("{folder}/csv/{folder}_{x}.csv"))
                          saveRDS(games, glue::glue("{folder}/rds/{folder}_{x}.rds"))
                          arrow::write_parquet(games,glue::glue("{folder}/parquet/{folder}_{x}.parquet"))
                          
                        })




git2r::add(repo, glue::glue("{folder}/*")) # add specific files to staging of commit
git2r::commit(repo, message = glue::glue("Updated {folder} at {Sys.time()} using cfbfastR")) # commit the staged files with the chosen message
git2r::pull(repo) # pull repo (and pray there are no merge commits)
git2r::push(repo, credentials = git2r::cred_user_pass(username = Sys.getenv("ghub"), password = Sys.getenv("gh_pw"))) # push commit

message(paste('Successfully uploaded to GitHub values as of',Sys.time())) # I have cron set up to pipe this message to healthchecks.io so that I can keep track if something is broken
