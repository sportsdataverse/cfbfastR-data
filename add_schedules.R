library(cfbfastR)
library(git2r)
library(dplyr)
library(arrow)
library(purrr)
library(glue)


repo <- git2r::repository('./') 
games <- purrr::map_dfr(2001:2020, 
                        function(x){
                          games <- cfbfastR::cfbd_game_info(x)
                          write.csv(games, glue::glue("schedules/csv/schedules_{x}.csv"), row.names = FALSE)
                          saveRDS(games, glue::glue("schedules/rds/schedules_{x}.rds"))
                          arrow::write_parquet(games,glue::glue("schedules/parquet/schedules_{x}.parquet"))
                          
                        })




git2r::add(repo, 'schedules/*') # add specific files to staging of commit
git2r::commit(repo, message = glue::glue("Updated schedules at {Sys.time()} using cfbfastR")) # commit the staged files with the chosen message
git2r::pull(repo) # pull repo (and pray there are no merge commits)
git2r::push(repo, credentials = git2r::cred_user_pass(username = Sys.getenv("ghub"), password = Sys.getenv("gh_pw"))) # push commit

message(paste('Successfully uploaded to GitHub values as of',Sys.time())) # I have cron set up to pipe this message to healthchecks.io so that I can keep track if something is broken
