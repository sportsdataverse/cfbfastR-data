library(lubridate)
library(utils)

message <- sprintf("Updated %s (ET) using cfbfastR version %s", lubridate::now("America/New_York"), utils::packageVersion("cfbfastR"))

system(glue::glue('git config --local user.email "actions@GitHub.com" '))
system(glue::glue('git config --local user.name "GitHub Actions"'))
system(glue::glue('git add .'))
system(glue::glue('git commit -am "{message}"'))
