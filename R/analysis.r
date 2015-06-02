
#' @export
repo_language <- function(df) {
  df %>%
    group_by(language) %>%
    summarise(size = sum(size))
}

#' @export
commits_per_week <- function(df) {
  df %>%
    mutate(weekdate = floor_date(as.Date(date), "week")) %>%
    group_by(weekdate) %>%
    summarise(commits = n())
}

#' @export
projects_over_time <- function(df) {
  df %>%
    group_by(repo, date = floor_date(as.Date(date), "week")) %>%
    summarise(commits = n()) %>%
    mutate(max_commits = max(commits)) %>%
    mutate(percentage = round(ceiling(commits/max_commits*10)))
}

#' @export
author_contributions <- function(df) {
  df %>%
    group_by(name) %>%
    summarise(commits = n()) %>%
    arrange(desc(commits))
}
