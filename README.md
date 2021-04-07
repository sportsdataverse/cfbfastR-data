## __cfbfastR-data__ 
[![Twitter Follow](https://img.shields.io/twitter/follow/cfbfastR?color=blue&label=%40cfbfastR&logo=twitter&style=for-the-badge)](https://twitter.com/cfbfastR) 
[![Twitter Follow](https://img.shields.io/twitter/follow/saiemgilani?color=blue&label=%40saiemgilani&logo=twitter&style=for-the-badge)](https://twitter.com/saiemgilani) 
<a href="https://github.com/saiemgilani" target="blank"><img src="https://img.shields.io/github/followers/saiemgilani?color=eee&logo=Github&style=for-the-badge" alt="@saiemgilani" /></a>

### __cfbfastR data 2014-2020__

## RDS
```
seasons <- 2014:2020
pbp <- purrr::map_df(seasons, function(x) {
  readRDS(
    url(
      glue::glue("https://raw.githubusercontent.com/saiemgilani/cfbfastR-data/master/data/rds/pbp_players_pos_{x}.rds")
    )
  )
})
```

## CSV (compressed)

This has been removed from the repository on account of the size being too large.

## Parquet (arrow)
```
seasons <- 2014:2020
pbp <- purrr::map_df(seasons, function(x) {
  download.file(glue::glue("https://raw.githubusercontent.com/saiemgilani/cfbfastR-data/master/data/parquet/pbp_players_pos_{x}.parquet"),"tmp.parquet")
  df <- arrow::read_parquet("tmp.parquet")
  return(df)
})
```
