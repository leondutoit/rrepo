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

#' @export
more_repos <- function(nextrel) {
  is.na(str_extract(nextrel, "page=1"))
}

#' @export
combine_repo_data <- function(dfs) {
  tbl_df(
    rbind_all(
      Map(function(x) {
        tbl_df(x) %>%
          select(
            name, description, created_at, updated_at,
            pushed_at, size, language, forks_count,
            url, ssh_url, clone_url)
        },
      dfs))) %>%
    arrange(desc(updated_at))
}

#' @export
get_repo_data <- function(url, auth) {
  print("fetching repo info")
  resp <- doget(url, auth)
  dfs <- list(fromJSON(resp[['data']]))
  nextrel <- get_next_rel(resp)
  i <- 2
  while (more_repos(nextrel)) {
    resp <- doget(nextrel, auth)
    dfs[[i]] <- fromJSON(resp[['data']])
    nextrel <- get_next_rel(resp)
    i <- i + 1
  }
  combine_repo_data(dfs)
}

#' @export
repo_download_list <- function(df, exclude = "") {
  df %>%
    filter(!description %in% exclude) %>%
    select(url) %>%
      mutate(download_url = paste(url, '/commits?page=1?per_page=100', sep = '')) %>%
      select(download_url)
}

#' @export
parse_commits_api <- function(df) {
  tbl_df(
    data.frame(
      sha = df[['sha']],
      name = df[['url']],
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
get_commit_data_from_api <- function(url, write = FALSE) {
  print("fetching commits...")
  print(url)
  print(1)
  resp <- doget(url)
  commits <- list(parse_commits_api(fromJSON(resp[['data']])))
  nextrel <- get_next_rel(resp)
  i <- 2
  while (str_detect(nextrel, "last_sha")) {
    print(i)
    resp <- doget(nextrel)
    commits[[i]] <- parse_commits_api(fromJSON(resp[['data']]))
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
repo_name <- function(x) {
  unlist(Map(function(x) {
      unlist(strsplit(x, "/"))[6]
    }, x), use.names = FALSE)
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
clone_repos <- function(repo_data) {
  info <- repo_data %>%
    select(name, clone_url) %>%
    mutate(clone_statement = paste('git clone', clone_url))
  Map(function(name, statement) {
      already_cloned <- name %in% dir()
      if (already_cloned) {
        print("not cloning, already exists")
      } else {
        system(statement)
      }
    },
    info[['name']],
    info[['clone_statement']])
}

#' @export
extract_git_log <- function() {
  git_log <- paste("git log --no-merges --shortstat",
    "--pretty=format:'%H;%an;%ae;%ai;%f^'",
    "| sed -e :begin -e '$!N;s/\\^\\n/\\; /; tbegin'",
    "| grep -v Merge")
  glog <- system(git_log, intern = TRUE)
  glog <- glog[str_detect(glog, ";")]
  gcl <- strsplit(glog, ";")
  rbind_all(
    Map(function(x, y) {
      data.frame(t(gcl[[y]]),
        stringsAsFactors = FALSE)
      },
    gcl,
    seq_along(gcl)))
}

#' @export
commit_changes <- function(data, type) {
  unlist(Map(function(x) {
      changes <- unlist(strsplit(x, ","));
      isthere <- str_detect(changes, type)
      if (TRUE %in% isthere) {
        change <- changes[isthere]
        as.numeric(str_replace_all(change, "[(a-z)+-]", ""))
      } else {
        0
      }
    },
    data),
  use.names = FALSE)
}

files <- function(data) {
  commit_changes(data, "file")
}

insertions <- function(data) {
  commit_changes(data, "insertions")
}

deletions <- function(data) {
  commit_changes(data, "deletions")
}

#' @export
parse_changes <- function(commits) {
  commits %>%
    mutate(
      file_changes = files(changes),
      insertions = insertions(changes),
      deletions = deletions(changes))
}

#' @export
get_commit_data_from_local <- function(repo_data, also_clone = FALSE) {
  if (also_clone) {
    clone_repos(repo_data)
  }
  repo_list <- repo_data$name
  data <- list()
  Map(function(x, y) {
      setwd(x)
      df <- extract_git_log()
      df$repo_name <- x
      data[[y]] <<- df
      setwd("../")
    },
    repo_list,
    seq_along(repo_list))
  data
}

#' @export
get_all_commit_data <- function(repo_data, api = TRUE) {
  if (api) {
    repo_list <- repo_download_list(repo_data)
    data <- Map(get_commit_data_from_api, repo_list, TRUE)
    commits <- tbl_df(rbind_all(data))
  } else {
    data <- get_commit_data_from_local(repo_data)
    commits <- tbl_df(rbind_all(data))
    names(commits) <- c("sha", "author", "email",
      "date", "message", "changes", "repo_name")
    commits <- parse_changes(commits)
    }
  commits
}
