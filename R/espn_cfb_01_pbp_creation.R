rm(list = ls())
gcol <- gc()
# lib_path <- Sys.getenv("R_LIBS")
# if (!requireNamespace("pacman", quietly = TRUE)) {
#   install.packages("pacman", lib = Sys.getenv("R_LIBS"), repos = "http://cran.us.r-project.org")
# }
suppressPackageStartupMessages(suppressMessages(library(dplyr)))
suppressPackageStartupMessages(suppressMessages(library(magrittr)))
suppressPackageStartupMessages(suppressMessages(library(jsonlite)))
suppressPackageStartupMessages(suppressMessages(library(purrr)))
suppressPackageStartupMessages(suppressMessages(library(progressr)))
suppressPackageStartupMessages(suppressMessages(library(data.table)))
suppressPackageStartupMessages(suppressMessages(library(arrow)))
suppressPackageStartupMessages(suppressMessages(library(glue)))
suppressPackageStartupMessages(suppressMessages(library(optparse)))


option_list <- list(
  make_option(c("-s", "--start_year"),
              action = "store",
              default = cfbfastR:::most_recent_cfb_season(),
              type = "integer",
              help = "Start year of the seasons to process"),
  make_option(c("-e", "--end_year"),
              action = "store",
              default = cfbfastR:::most_recent_cfb_season(),
              type = "integer",
              help = "End year of the seasons to process")
)
opt <- parse_args(OptionParser(option_list = option_list))
options(stringsAsFactors = FALSE)
options(scipen = 999)
years_vec <- opt$s:opt$e

# --- compile into play_by_play_{year}.parquet ---------
cfb_pbp_games <- function(y) {

  espn_df <- data.frame()
  sched <- cfbfastR:::rds_from_url(paste0("https://raw.githubusercontent.com/sportsdataverse/cfbfastR-raw/main/cfb/schedules/rds/cfb_schedule_", y, ".rds"))
  ifelse(!dir.exists(file.path("cfb/schedules")), dir.create(file.path("cfb/schedules")), FALSE)
  ifelse(!dir.exists(file.path("cfb/schedules/rds")), dir.create(file.path("cfb/schedules/rds")), FALSE)
  ifelse(!dir.exists(file.path("cfb/schedules/parquet")), dir.create(file.path("cfb/schedules/parquet")), FALSE)
  saveRDS(sched, glue::glue("cfb/schedules/rds/cfb_schedule_{y}.rds"))
  arrow::write_parquet(sched, glue::glue("cfb/schedules/parquet/cfb_schedule_{y}.parquet"))

  season_pbp_list <- sched %>%
    dplyr::filter(.data$game_json == TRUE) %>%
    dplyr::pull("game_id")

  if (length(season_pbp_list) > 0) {

    cli::cli_progress_step(msg = "Compiling {y} ESPN CFB pbps ({length(season_pbp_list)} games)",
                           msg_done = "Compiled {y} ESPN CFB pbps!")

    future::plan("multisession")
    espn_df <- furrr::future_map_dfr(season_pbp_list, function(x) {
      tryCatch(
        expr = {
          game_json <- glue::glue("https://raw.githubusercontent.com/sportsdataverse/cfbfastR-raw/main/cfb/json/final/{x}.json") %>%
            jsonlite::fromJSON()

          date <- game_json[['header']][['competitions']][['date']]

          pbp <- game_json %>%
            purrr::pluck("plays") %>%
            jsonlite::toJSON() %>%
            jsonlite::fromJSON(flatten = TRUE)

          pbp$date <- date
          return(pbp)
        },
        error = function(e) {
          message(glue::glue("{Sys.time()}: game_id {x} PBP issue with error {e}"))
        }
      )
    }, .options = furrr::furrr_options(seed = TRUE))

    cli::cli_progress_step(msg = "Updating {y} ESPN CFB PBP GitHub Release",
                           msg_done = "Updated {y} ESPN CFB PBP GitHub Release!")

  }

  if (nrow(espn_df) > 1) {

    suppressWarnings(
      espn_df <- espn_df %>%
        dplyr::mutate(dplyr::across(dplyr::any_of(c(
          "id",
          "game_id",
          "type_id",
          "status_type_id",
          "home_id",
          "home_venue_id",
          "home_conference_id",
          "home_score",
          "away_id",
          "away_venue_id",
          "away_conference_id",
          "away_score",
          "season",
          "season_type",
          "groups_id",
          "tournament_id",
          "venue_id"
        )), ~as.integer(.x))) %>%
        dplyr::mutate(
          game_date_time = lubridate::ymd_hm(substr(.data$date, 1, nchar(.data$date) - 1)) %>%
            lubridate::with_tz(tzone = "America/New_York"),
          game_date = as.Date(substr(.data$game_date_time, 1, 10)))
    )

    espn_df <- espn_df %>%
      dplyr::arrange(dplyr::desc(.data$game_date)) %>%
      cfbfastR:::make_cfbfastR_data("ESPN CFB Play-by-Play from cfbfastR data repository", Sys.time())

    ifelse(!dir.exists(file.path("cfb/pbp")), dir.create(file.path("cfb/pbp")), FALSE)

    ifelse(!dir.exists(file.path("cfb/pbp/rds")), dir.create(file.path("cfb/pbp/rds")), FALSE)
    saveRDS(espn_df, glue::glue("cfb/pbp/rds/play_by_play_{y}.rds"))

    ifelse(!dir.exists(file.path("cfb/pbp/parquet")), dir.create(file.path("cfb/pbp/parquet")), FALSE)
    arrow::write_parquet(espn_df, glue::glue("cfb/pbp/parquet/play_by_play_{y}.parquet"))

    sportsdataversedata::sportsdataverse_save(
      data_frame = espn_df,
      file_name =  glue::glue("play_by_play_{y}"),
      sportsdataverse_type = "play-by-play data",
      release_tag = "espn_cfb_pbp",
      pkg_function = "cfbfastR::load_cfb_pbp()",
      file_types = c("rds", "csv", "parquet"),
      .token = Sys.getenv("GITHUB_PAT")
    )
  }

  sched <- sched %>%
    dplyr::mutate(dplyr::across(dplyr::any_of(c(
      "id",
      "game_id",
      "type_id",
      "status_type_id",
      "home_id",
      "home_venue_id",
      "home_conference_id",
      "home_score",
      "away_id",
      "away_venue_id",
      "away_conference_id",
      "away_score",
      "season",
      "season_type",
      "groups_id",
      "tournament_id",
      "venue_id"
    )), ~as.integer(.x))) %>%
    dplyr::mutate(
      status_display_clock = as.character(.data$status_display_clock),
      game_date_time = lubridate::ymd_hm(substr(.data$date, 1, nchar(.data$date) - 1)) %>%
        lubridate::with_tz(tzone = "America/New_York"),
      game_date = as.Date(substr(.data$game_date_time, 1, 10)))

  if (nrow(espn_df) > 0) {

    sched <- sched %>%
      dplyr::mutate(
        PBP = ifelse(.data$game_id %in% unique(espn_df$game_id), TRUE, FALSE))

  } else {

    cli::cli_alert_info("{length(season_pbp_list)} ESPN CFB pbps to be compiled for {y}, skipping PBP compilation")
    sched$PBP <- FALSE

  }

  final_sched <- sched %>%
    dplyr::distinct() %>%
    dplyr::arrange(dplyr::desc(.data$date))

  final_sched <- final_sched %>%
    cfbfastR:::make_cfbfastR_data("ESPN CFB Schedule from cfbfastR data repository", Sys.time())

  ifelse(!dir.exists(file.path("cfb/schedules")), dir.create(file.path("cfb/schedules")), FALSE)
  ifelse(!dir.exists(file.path("cfb/schedules/rds")), dir.create(file.path("cfb/schedules/rds")), FALSE)
  ifelse(!dir.exists(file.path("cfb/schedules/parquet")), dir.create(file.path("cfb/schedules/parquet")), FALSE)

  saveRDS(final_sched, glue::glue("cfb/schedules/rds/cfb_schedule_{y}.rds"))
  arrow::write_parquet(final_sched, glue::glue("cfb/schedules/parquet/cfb_schedule_{y}.parquet"))
  rm(sched)
  rm(final_sched)
  rm(espn_df)
  gc()
  return(NULL)
}

tictoc::tic()
all_games <- purrr::map(years_vec, function(y) {
  cfb_pbp_games(y)
  return(NULL)
})
tictoc::toc()

cli::cli_progress_step(msg = "Compiling ESPN CFB master schedule",
                       msg_done = "ESPN CFB master schedule compiled and written to disk")

sched_list <- list.files(path = glue::glue("cfb/schedules/rds/"))
sched_g <-  purrr::map_dfr(sched_list, function(x) {
  sched <- readRDS(paste0("cfb/schedules/rds/", x)) %>%
    dplyr::mutate(dplyr::across(dplyr::any_of(c(
      "id",
      "game_id",
      "type_id",
      "status_type_id",
      "home_id",
      "home_venue_id",
      "home_conference_id",
      "home_score",
      "away_id",
      "away_venue_id",
      "away_conference_id",
      "away_score",
      "season",
      "season_type",
      "groups_id",
      "tournament_id",
      "venue_id"
    )), ~as.integer(.x))) %>%
    dplyr::mutate(
      status_display_clock = as.character(.data$status_display_clock),
      game_date_time = lubridate::ymd_hm(substr(.data$date, 1, nchar(.data$date) - 1)) %>%
        lubridate::with_tz(tzone = "America/New_York"),
      game_date = as.Date(substr(.data$game_date_time, 1, 10)))
  return(sched)
})

sched_g <- sched_g %>%
  cfbfastR:::make_cfbfastR_data("ESPN CFB Schedule from cfbfastR data repository", Sys.time())

# data.table::fwrite(sched_g %>% dplyr::arrange(desc(.data$date)), "cfb/cfb_schedule_master.csv")
# data.table::fwrite(sched_g %>%
#                      dplyr::filter(.data$PBP == TRUE) %>%
#                      dplyr::arrange(dplyr::desc(.data$date)), "cfb/cfb_games_in_data_repo.csv")

# arrow::write_parquet(sched_g %>%
#                        dplyr::arrange(dplyr::desc(.data$date)), glue::glue("cfb/cfb_schedule_master.parquet"))

# arrow::write_parquet(sched_g %>%
#                        dplyr::filter(.data$PBP == TRUE) %>%
#                        dplyr::arrange(dplyr::desc(.data$date)), "cfb/cfb_games_in_data_repo.parquet")

cli::cli_progress_message("")


rm(sched_g)
rm(sched_list)
rm(years_vec)
gcol <- gc()
