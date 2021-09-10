message <- sprintf("Updated %s (ET) using cfbfastR version %s", lubridate::now("America/New_York"), utils::packageVersion("cfbfastR"))

git <- function(..., echo_cmd = TRUE, echo = TRUE, error_on_status = FALSE) {
  callr::run("git", c(...),
             echo_cmd = echo_cmd, echo = echo,
             error_on_status = error_on_status
  )
}
git("add","data/*","rosters/*","schedules/*","team_info/*","player_stats/*")
system(glue::glue('git config --local user.email "saiem.gilani@gmail.com" '))
system(glue::glue('git config --local user.name "Saiem Gilani"'))
system(glue::glue('git commit -m "{message}"'))
# git("commit", "-am", message)