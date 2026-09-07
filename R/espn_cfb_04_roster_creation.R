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
suppressPackageStartupMessages(suppressMessages(library(readr)))
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


games <- purrr::map(years_vec, function(x) {
  roster <- data.frame()
  roster <- cfbfastR::cfbd_team_roster(year = x)

  if (nrow(roster) > 0) {
    roster$season <- x
    # recruit_ids is a list-column and CFBD returns its elements with mixed
    # types -- measured on the 2026 roster: 18,515 integer vectors and 12,121
    # character vectors. csv and rds tolerate that; arrow cannot infer a single
    # list type and write_parquet() aborts with "Invalid: cannot convert",
    # which is what failed the 2026-09-07 run at espn_cfb_04_roster.
    #
    # Coercing every element to integer is lossless here: all 12,127 character
    # values are plain numeric id strings, with none non-coercible and no ""/NA.
    # The length-0 -> as.integer(0) behaviour is left alone so already-published
    # rosters keep their existing semantics.
    roster$recruit_ids <- lapply(roster$recruit_ids, function(y) {
      if (length(y) == 0) as.integer(0) else as.integer(y)
    })
    roster <- roster %>%
      cfbfastR:::make_cfbfastR_data("ESPN CFB Roster from cfbfastR data repository", Sys.time())

    cli::cli_progress_step(msg = "Updating {x} ESPN CFB PBP GitHub Release",
                          msg_done = "Updated {x} ESPN CFB PBP GitHub Release!")
    ifelse(!dir.exists(file.path("cfb")), dir.create(file.path("cfb")), FALSE)
    ifelse(!dir.exists(file.path("cfb/roster")), dir.create(file.path("cfb/roster")), FALSE)
    ifelse(!dir.exists(file.path("cfb/roster/csv")), dir.create(file.path("cfb/roster/csv")), FALSE)
    readr::write_csv(roster, glue::glue("cfb/roster/csv/roster_{x}.csv"))

    ifelse(!dir.exists(file.path("cfb/roster/rds")), dir.create(file.path("cfb/roster/rds")), FALSE)
    saveRDS(roster, glue::glue("cfb/roster/rds/roster_{x}.rds"))

    ifelse(!dir.exists(file.path("cfb/roster/parquet")), dir.create(file.path("cfb/roster/parquet")), FALSE)
    arrow::write_parquet(roster, glue::glue("cfb/roster/parquet/roster_{x}.parquet"))

    sportsdataversedata::sportsdataverse_save(
      data_frame = roster,
      file_name =  glue::glue("roster_{x}"),
      sportsdataverse_type = "roster data",
      release_tag = "espn_cfb_rosters",
      pkg_function = "cfbfastR::load_cfb_rosters()",
      file_types = c("rds", "csv", "parquet"),
      .token = Sys.getenv("GITHUB_PAT")
    )
    cli::cli_progress_message("")
  } else {
    cli::cli_alert_info("{Sys.time()}: No roster data available for {x}!")
  }
})

rm(list = ls())
gcol <- gc()