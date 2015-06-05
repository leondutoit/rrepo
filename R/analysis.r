
#' Show repo language stats
#'
#' \code{repo_language(org_repo_data)} return a summary of main languages of all repos
#'
#' This function is based on information from github and is not 100% accurate
#'
#' @param org_repo_data  (data frame) github data about an organisation's repo
#' @return a data frame of summary stats
#'
#' @export
repo_language <- function(org_repo_data) {
  org_repo_data %>%
    group_by(language) %>%
    summarise(size = sum(size))
}

#' Show commits per week per repo
#'
#' \code{commits_per_week(commits)}
#'
#' This function does what it says :)
#'
#' @param commits (data frame) commits for one or more repos
#' @return a data frame with aggegates about how many commits per week per repo
#'
#' @export
commits_per_week <- function(commits) {
  commits %>%
    mutate(weekdate = floor_date(as.Date(date), "week")) %>%
    group_by(weekdate) %>%
    summarise(commits = n())
}

#' Show relative commit activity per week over time for all repos
#'
#' \code{projects_over_time} returns summary stats
#'
#' This function shows relative activity (% of most commits per week, a bit random but yeah)
#'
#' @param (data frame) commits the commits of one or more repos
#' @return a data frame with summary stats
#'
#' @export
projects_over_time <- function(commits) {
  commits %>%
    group_by(repo, date = floor_date(as.Date(date), "week")) %>%
    summarise(commits = n()) %>%
    mutate(max_commits = max(commits)) %>%
    mutate(percentage = round(ceiling(commits/max_commits*10)))
}

#' Total commits per person
#'
#' \code{author_contributions} shows stats as the name suggests
#'
#' This function shows how many commits each contributor has made
#'
#' @param commits (data frame) the commits you want to inspect
#' @return a data frame of stats
#'
#' @export
author_contributions <- function(commits) {
  commits %>%
    group_by(name) %>%
    summarise(commits = n()) %>%
    arrange(desc(commits))
}
