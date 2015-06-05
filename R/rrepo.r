
#' @import dplyr
#' @import lubridate
#' @import jsonlite
#' @import stringr
#' @import httr
#' @import git2r
#' @import ggplot2


http_get <- function(url, auth) {
  resp <- GET(url, authenticate(auth$user, auth$pw))
  list(data = content(resp, "text"), headers = headers(resp))
}

#' @export
get_next_rel <- function(resp) {
  if ("link" %in% names(resp$headers)) {
    links <- resp$headers$link
    link <- unlist(strsplit(links, ";"))[1]
    str_replace_all(link, "([<>])", "")
  } else {
    "done"
  }
}

more_repos <- function(nextrel) {
  is.na(str_extract(nextrel, "page=1"))
}

parse_org_repo_data <- function(repo_data) {
  repo_data %>% select(name, created_at, updated_at, clone_url, language, size)
}

#' Get repository data for the organisation from github
#'
#' \code{get_org_repo_data} returns a data frame with repository information
#'
#' This function performs one or more HTTP requests aagainst the github API v3.
#'
#' @param url a string url
#' @param auth a list with two named fields list(user = "", pw = "")
#' @return a data frame with git repo information
#'
#' @export
get_org_repo_data <- function(url, auth) {
  print("fetching repo info")
  resp <- http_get(url, auth)
  repo_data <- parse_org_repo_data(fromJSON(resp$data))
  dfs <- list(repo_data)
  nextrel <- get_next_rel(resp)
  i <- 2
  while (more_repos(nextrel)) {
    resp <- http_get(nextrel, auth)
    dfs[[i]] <- parse_org_repo_data(fromJSON(resp$data))
    nextrel <- get_next_rel(resp)
    i <- i + 1
  }
  bind_rows(dfs)
}

#' Clone repos in repo_data
#'
#' \code{clone_org_repos} clones all repos in the given list and returns a list of references
#'
#' This function create a temporary folder to store local clones of repos
#'
#' @param org_data the return value of \code{get_org_repo_data}
#' @return a list of references to cloned git repositories
#'
#' @export
clone_org_repos <- function(org_data) {
  path <- file.path(tempfile("rrepo-"), "repos")
  dir.create(path)
  repos <- list()
  i <- 1
  for (url in org_data$clone_url) {
    repo <- clone(url, paste0(path, "/", org_data$name[[i]]))
    repos[[i]] <- repo
    i <- i + 1
  }
  repos
}

#' Remove cloned repos
#'
#' \code{remove_local_clones(repos)} deletes git repos from temporary folder
#'
#' This function loops through all references to repos and deltes them on the file-sysem.
#'
#' @param repos the return value of clone_org_repos - a list of references
#' @return nothing
#'
#' @export
remove_local_clones <- function(repos) {
  for (repo in repos) {
    system(paste("rm -rf", slot(repo, "path")))
  }
  rm(repos)
}

make_date <- function(x) {
  as.Date(as.POSIXct(x, origin = "1970-01-01"))
}

setGeneric("commit_info", function(x) {
  stardardGeneric("commit_info")
})

setMethod("commit_info",
  c(x = "git_commit"),
  function(x) {
    sha <- slot(x, "sha")
    author <- slot(x, "author")
    name <- slot(author, "name")
    email <- slot(author, "email")
    date <- make_date(slot(slot(author, "when"), "time"))
    message <- slot(x, "summary")
  data_frame(
    sha = sha,
    name = name,
    email = email,
    date = date,
    message = message)
})

#' Extract the git log for a repo
#'
#' \code{git_log_to_df(repo)} extracts the commit log and other useful information
#'
#' This function operates on a git2r repo refernce
#'
#' @param repo a git2r reference to a local repo
#' @return a data frame with all commits and metadata
#'
#' @export
git_log_to_df <- function(repo) {
  commit_log <- commits(repo)
  commit_list <- list()
  for (i in seq_along(commit_log)) {
    commit_list[[i]] <- commit_info(commit_log[[i]])
  }
  df <- bind_rows(commit_list)
  df$repo <- basename(slot(repo, "path"))
  df
}

#' Extract and combine git log for all repos in the list of references
#'
#' \code{get_all_commit_data(repos)} gets all commit data :)
#'
#' This function combines all commit information from a list of repos into one data frame
#'
#' @param a list of git2r repo references
#' @return a data frame of commits and associated information
#'
#' @export
get_all_commit_data <- function(repos) {
  all_commits <- list()
  for (i in seq_along(repos)) {
    all_commits[[i]] <- git_log_to_df(repos[[i]])
  }
  bind_rows(all_commits)
}
