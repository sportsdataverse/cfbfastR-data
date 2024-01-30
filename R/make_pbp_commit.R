message <- glue("Updated {lubridate::now('America/New_York')} (ET) using cfbfastR version {utils::packageVersion('cfbfastR')}")

system("git config --local user.email 'actions@GitHub.com'")
system("git config --local user.name 'GitHub Actions'")
system("git add .")
system(glue("git commit -am '{message}'"))
