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
get_repo_data <- function(url, auth) {
  # url: "https://api.github.com/orgs/{org_name}/repos?page=1&per_page=100"
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
  tbl_df(rbind_all(
    Map(function(x) {
        tbl_df(x) %.%
          select(name, description, created_at, updated_at,
            pushed_at, size, language, forks_count,
            url, ssh_url, clone_url)
        },
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
parse_commits_api <- function(df) {
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
clone_repos <- function(repo_data) {
  info <- repo_data %.%
    select(name, clone_url) %.%
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
  # assumes already in repo dir
  # does this for current repo
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

# TODO check
#' @export
get_commit_data_from_local <- function(repo_data) {
  clone_repos(repo_data)
  repo_list <- repolist(repo_data)
  Map(function(x) {
    setwd(x);
    extract_git_log();
    setwd("..") # figure out
    },
    repo_list)
}

# TODO: check that this works
#' @export
get_all_commit_data <- function(repo_data, api = TRUE) {
  if (api) {
    repo_list <- repolist(repo_data)
    data <- Map(get_commit_data_from_api, repo_list, TRUE)
    tbl_df(rbind_all(data))
  } else {
    data <- get_commit_data_from_local(repo_data)
    # set names
    tbl_df(rbind_all(data))
    }

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


