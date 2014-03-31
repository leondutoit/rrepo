
#' @export
over_time <- function(df) {
  df %.%
    group_by(name, email, date = floor_date(as.Date(date), "week")) %.%
    summarise(commits = n())
}

#' @export
commits_per_week <- function(df) {
  df %.%
    mutate(weekdate = floor_date(as.Date(date), "week")) %.%
    group_by(weekdate) %.%
    summarise(commits = n())
}

#' @export
projects <- function(df) {
  projects <- df %.%
    group_by(name, date = floor_date(as.Date(date), "week")) %.%
    summarise(commits = n())
  projects %.%
    mutate(max_commits = max(commits)) %.%
    mutate(percentage = round(ceiling(commits/max_commits*10)))
}


