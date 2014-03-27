#' rrepo
#' github API v3

#' @import dplyr
#' @import RCurl
#' @import lubridate
#' @import ggplot2
#' @import jsonlite
#' @import stringr
#' @import scales

#' @export
doget <- function(url, auth) {
  # auth "username:password"
  options(
    RCurlOptions = list(
      followlocation = TRUE,
      timeout = 100,
      useragent = "curl/7.30.0"))
  h <- basicHeaderGatherer()
  resp <- getURL(
    url,
    userpwd = auth,
    httpauth = 1L,
    headerfunction = h$update)
  list(
    data = resp,
    header = h$value())
}

#' @export
get_next_rel <- function(resp) {
  headers <- names(resp[['header']])
  if ("Link" %in% headers) {
    links <- resp[['header']][['Link']]
    link <- unlist(strsplit(links, ";"))[1]
    str_replace_all(link, "([<>])", "")
  } else {
    "done"
  }
}

more_repos <- function(nextrel) {
  is.na(str_extract(nextrel, "page=1"))
}

#' @export
get_repo_data <- function(url) {
  # url: "https://api.github.com/orgs/{org_name}/repos?page=1&per_page=100"
  print("fetching repo info")
  resp <- doget(url)
  dfs <- list(fromJSON(resp[['data']]))
  nextrel <- get_next_rel(resp)
  i <- 2
  while (more_repos(nextrel)) {
    resp <- doget(nextrel)
    dfs[[i]] <- fromJSON(resp[['data']])
    nextrel <- get_next_rel(resp)
    i <- i + 1
  }
  tbl_df(rbind_all(
    Map(function(x) {
        tbl_df(x) %.%
          select(name, description, created_at, updated_at,
            pushed_at, size, language, forks_count, url) },
        dfs)))
}

#' @export
repo_language <- function(df) {
  df %.%
    group_by(language) %.%
    summarise(size = sum(size))
}

#' @export
repolist <- function(df, exclude = "") {
  # exclude c("dontcounthitsrepo", "orthisone")
  df %.%
    filter(!description %in% exclude) %.%
    select(url) %.%
    mutate(download_url = paste(url, '/commits?page=1?per_page=100', sep = '')) %.%
    select(download_url)
}

#' @export
parse_commits <- function(df) {
  tbl_df(
    data.frame(
      sha = df[['sha']],
      repo = df[['url']],
      email = df[['commit']][['author']][['email']],
      date = df[['commit']][['author']][['date']],
      message = df[['commit']][['message']],
      stringsAsFactors = FALSE))
}

#' @export
write_out <- function(data, url) {
  name <- paste(unlist(strsplit(url, "/"))[6], ".csv", sep = "")
  write.csv(data, name, row.names = FALSE)
}

#' @export
get_commit_data <- function(url, write = FALSE) {
  print("fetching commits...")
  print(url)
  print(1)
  resp <- doget(url)
  commits <- list(parse_commits(fromJSON(resp[['data']])))
  nextrel <- get_next_rel(resp)
  i <- 2
  while (str_detect(nextrel, "last_sha")) {
    print(i)
    resp <- doget(nextrel)
    commits[[i]] <- parse_commits(fromJSON(resp[['data']]))
    nextrel <- get_next_rel(resp)
    i <- i + 1
  }
  data <- tbl_df(rbind_all(commits))
  if (write) {
    write_out(data, url)
  }
  data
}

#' @export
get_all_commit_data <- function(repo_list) {
  data <- Map(get_commit_data, repo_list, TRUE)
  tbl_df(rbind_all(data))
}

#' @export
repo_name <- function(x) {
  unlist(Map(function(x) {
      unlist(strsplit(x, "/"))[6]
    }, x))
}

#' @export
clean_github <- function(df, exclude = "") {
  df %.%
    mutate(name = repo_name(repo)) %.%
    select(sha, name, email, date, message) %.%
    filter(!name %in% exclude)
}

#' @export
combine_from_csv <- function() {
  files <- system('ls | grep csv', intern = TRUE)
  dfs <- list()
  for (i in seq_along(files)) {
    print(files[i])
    dfs[[i]] <- read.csv(files[i], stringsAsFactors = FALSE)
  }
  tbl_df(rbind_all(dfs))
}

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

#' @export
project_heatmap <- function(df) {
  colours <- c( "#a50026", "#d73027", "#f46d43", "#fdae61",
    "#fee090", "#e0f3f8", "#abd9e9", "#74add1", "#4575b4", "#313695", "#313695")
  labels <- as.character(10 * 1:10)
  ggplot(df, aes(
    date, name, fill = as.factor(ceiling(percentage)))) +
      geom_raster() +
      scale_fill_manual(
        values = colours,
        labels = labels,
        name = "percentage")
}
