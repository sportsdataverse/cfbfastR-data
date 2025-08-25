#--- cfbfastR Data -----
piggyback::pb_release_create(
  repo = "sportsdataverse/sportsdataverse-data",
  tag = "cfbfastR_cfb_pbp",
  name = "cfbfastR_cfb_pbp",
  body = "NCAA College Football PBP Data with EPA/WPA (from cfbfastR)",
  .token = Sys.getenv("GITHUB_PAT")
)


#--- ESPN CFB Data -----
piggyback::pb_release_create(
  repo = "sportsdataverse/sportsdataverse-data",
  tag = "espn_cfb_schedules",
  name = "espn_cfb_schedules",
  body = "NCAA College Football Schedules Data (from ESPN)",
  .token = Sys.getenv("GITHUB_PAT")
)

piggyback::pb_release_create(
  repo = "sportsdataverse/sportsdataverse-data",
  tag = "espn_cfb_team_boxscores",
  name = "espn_cfb_team_boxscores",
  body = "NCAA College Football Team Boxscores Data (from ESPN)",
  .token = Sys.getenv("GITHUB_PAT")
)

piggyback::pb_release_create(
  repo = "sportsdataverse/sportsdataverse-data",
  tag = "espn_cfb_player_boxscores",
  name = "espn_cfb_player_boxscores",
  body = "NCAA College Football Player Boxscores Data (from ESPN)",
  .token = Sys.getenv("GITHUB_PAT")
)


piggyback::pb_release_create(
  repo = "sportsdataverse/sportsdataverse-data",
  tag = "espn_cfb_pbp",
  name = "espn_cfb_pbp",
  body = "NCAA College Football Play-by-Play Data (from ESPN)",
  .token = Sys.getenv("GITHUB_PAT")
)


piggyback::pb_release_create(
  repo = "sportsdataverse/sportsdataverse-data",
  tag = "espn_cfb_rosters",
  name = "espn_cfb_rosters",
  body = "NCAA College Football Roster Data (from ESPN)",
  .token = Sys.getenv("GITHUB_PAT")
)
