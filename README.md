## __cfbfastR-data__ 
[![Twitter Follow](https://img.shields.io/twitter/follow/cfbfastR?style=social)](https://twitter.com/cfbfastR) [![Twitter Follow](https://img.shields.io/twitter/follow/saiemgilani?style=social)](https://twitter.com/saiemgilani)

![social_card_data_repo.png](https://raw.githubusercontent.com/saiemgilani/cfbfastR-data/master/social_card_data_repo.png)
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
