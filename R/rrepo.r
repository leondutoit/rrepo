#' rrepo
#' github API v3

#' @import dplyr
#' @import lubridate
#' @import jsonlite
#' @import stringr
#' @import httr
#' @import git2r

#' @export
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
  repo_data %>% select(name, created_at, updated_at, clone_url, language)
}

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

remove_local_clones <- function(repos) {
  # todo impl
}

#' @export
setGeneric("commit_info", function(x) {
  stardardGeneric("commit_info")
})

#' @export
setMethod("commit_info",
  c(x = "git_commit"),
  function(x) {
    sha <- slot(x, "sha")
    author <- slot(x, "author")
    name <- slot(author, "name")
    email <- slot(author, "email")
    date <- slot(slot(author, "when"), "time") # TODO cast to posixct
    message <- slot(x, "summary")
  data_frame(
    sha = sha,
    name = name,
    email = email,
    date = date,
    message = message)
})

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

#' @export
get_all_commit_data <- function(repos) {
  all_commits <- list()
  for (i in seq_along(repos)) {
    all_commits[[i]] <- git_log_to_df(repos[[i]])
  }
  bind_rows(all_commits)
}
