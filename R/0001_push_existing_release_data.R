lib_path <- Sys.getenv("R_LIBS")
if (!requireNamespace("pacman", quietly = TRUE)) {
  install.packages("pacman", lib = Sys.getenv("R_LIBS"), repos = "http://cran.us.r-project.org")
}
suppressPackageStartupMessages(suppressMessages(library(dplyr, lib.loc = lib_path)))
suppressPackageStartupMessages(suppressMessages(library(magrittr, lib.loc = lib_path)))
suppressPackageStartupMessages(suppressMessages(library(jsonlite, lib.loc = lib_path)))
suppressPackageStartupMessages(suppressMessages(library(purrr, lib.loc = lib_path)))
suppressPackageStartupMessages(suppressMessages(library(progressr, lib.loc = lib_path)))
suppressPackageStartupMessages(suppressMessages(library(data.table, lib.loc = lib_path)))
suppressPackageStartupMessages(suppressMessages(library(arrow, lib.loc = lib_path)))
suppressPackageStartupMessages(suppressMessages(library(glue, lib.loc = lib_path)))
suppressPackageStartupMessages(suppressMessages(library(optparse, lib.loc = lib_path)))



  cfbfastR_pbp_list <- list.files(path = glue::glue("data/rds/"))
  cfbfastR_cfb_pbp_g <- purrr::map(cfbfastR_pbp_list, function(x) {
    cfbfastR_cfb_pbp <- readRDS(paste0("data/rds/", x)) %>%
      dplyr::mutate(
        game_id = as.integer(.data$game_id)
      )
    cfbfastR_cfb_pbp <- cfbfastR_cfb_pbp  %>% 
      cfbfastR:::make_cfbfastR_data("PBP from data repo and CollegeFootballData.com",Sys.time())
    
    y <- stringr::str_extract(x, "\\d+")
    sportsdataversedata::sportsdataverse_save(
      pkg_function = "cfbfastR::load_cfb_pbp()",
      data_frame = cfbfastR_cfb_pbp,
      file_name =  glue::glue("play_by_play_{y}"),
      sportsdataverse_type = "play-by-play data",
      release_tag = "cfbfastR_cfb_pbp",
      file_types = c("rds", "csv", "parquet"),
      .token = Sys.getenv("GITHUB_PAT")
    )
  })
  rm(cfbfastR_cfb_pbp_g)
  
  
  sched_list <- list.files(path = glue::glue("cfb/schedules/rds/"))
  sched_g <-  purrr::map(sched_list, function(x) {
    sched <- readRDS(paste0("cfb/schedules/rds/", x)) %>%
      dplyr::mutate(
        id = as.integer(.data$id),
        game_id = as.integer(.data$game_id),
        status_display_clock = as.character(.data$status_display_clock)
      )
    
    sched <- sched %>%
      cfbfastR:::make_cfbfastR_data("ESPN CFB Schedule from cfbfastR data repository", Sys.time())
    y <- stringr::str_extract(x, "\\d+")
    sportsdataversedata::sportsdataverse_save(
      data_frame = sched,
      file_name =  glue::glue("cfb_schedule_{y}"),
      sportsdataverse_type = "schedule data",
      release_tag = "espn_cfb_schedules",
      file_types = c("rds", "csv", "parquet"),
      .token = Sys.getenv("GITHUB_PAT")
    )
  })
  rm(sched_g)
  
  pbp_list <- list.files(path = glue::glue("cfb/pbp/rds/"))
  pbp_g <-  purrr::map(pbp_list, function(x) {
    pbp <- readRDS(paste0("cfb/pbp/rds/", x))
    
    pbp <- pbp %>%
      cfbfastR:::make_cfbfastR_data("ESPN CFB Play-by-Play from cfbfastR data repository", Sys.time())
    y <- stringr::str_extract(x, "\\d+")
    sportsdataversedata::sportsdataverse_save(
      data_frame = pbp,
      file_name =  glue::glue("play_by_play_{y}"),
      sportsdataverse_type = "Play-by-Play data",
      release_tag = "espn_cfb_pbp",
      file_types = c("rds", "csv", "parquet"),
      .token = Sys.getenv("GITHUB_PAT")
    )
  })
  rm(pbp_g)
  
  team_box_list <- list.files(path = glue::glue("cfb/team_box/rds/"))
  team_box_g <-  purrr::map(team_box_list, function(x) {
    team_box <- readRDS(paste0("cfb/team_box/rds/", x))
    team_box <- team_box %>%
      cfbfastR:::make_cfbfastR_data("ESPN CFB Team Boxscores from cfbfastR data repository", Sys.time())
    y <- stringr::str_extract(x, "\\d+")
    sportsdataversedata::sportsdataverse_save(
      data_frame = team_box,
      file_name =  glue::glue("team_box_{y}"),
      sportsdataverse_type = "Team Boxscores data",
      release_tag = "espn_cfb_team_boxscores",
      file_types = c("rds", "csv", "parquet"),
      .token = Sys.getenv("GITHUB_PAT")
    )
  })
  
  rm(team_box_g)
  
  player_box_list <- list.files(path = glue::glue("cfb/player_box/rds/"))
  player_box_g <-  purrr::map(player_box_list, function(x) {
    player_box <- readRDS(paste0("cfb/player_box/rds/", x))
    player_box <- player_box %>%
      cfbfastR:::make_cfbfastR_data("ESPN CFB Player Boxscores from cfbfastR data repository", Sys.time())
    y <- stringr::str_extract(x, "\\d+")
    sportsdataversedata::sportsdataverse_save(
      data_frame = player_box,
      file_name =  glue::glue("player_box_{y}"),
      sportsdataverse_type = "Player Boxscores data",
      release_tag = "espn_cfb_player_boxscores",
      file_types = c("rds", "csv", "parquet"),
      .token = Sys.getenv("GITHUB_PAT")
    )
  })
  
  rm(player_box_g)
  
