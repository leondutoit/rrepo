library(testthat)

create_test_response <- function(any_left = TRUE) {
  if (any_left) {
    header <- c(
        "Link" = paste(
          "<https://api.github.com/organizations/513560/repos?info=IAMNEXT>;",
          "rel=\"next\", ",
          "<https://api.github.com/organizations/513560/repos?page=2&per_page=20>;",
          " rel=\"last\"", sep = ""))
  } else {
    header <- c("None" = c("nothing left to request"))
  }
  list(
    data = data.frame(
      x = "irrelevant",
      stringsAsFactors = FALSE),
    header = header)
}

context("extracting the next URL for page traversal")

test_that("get_next_rel returns the next download url from the header", {
    test_response <- create_test_response(any_left = TRUE)
    correct_nextrel <- "https://api.github.com/organizations/513560/repos?info=IAMNEXT"
    nextrel <- get_next_rel(test_response)
    expect_equal(nextrel, correct_nextrel)
  }
)

test_that("get_next_rel returns 'done' when nothing left to download", {
    test_response <- create_test_response(any_left = FALSE)
    correct_nextrel <- "done"
    nextrel <- get_next_rel(test_response)
    expect_equal(nextrel, correct_nextrel)
  }
)

context("getting repo data from the API")

create_repo_dfs <- function() {
  testdf <- function(name, updated_at) {
    data.frame(
      name = c(name),
      description = c("test repo"),
      created_at = c(as.POSIXct("2014-04-01 13:09:11")),
      updated_at = c(as.POSIXct(updated_at)),
      pushed_at = c(as.POSIXct("2014-03-01 13:09:11")),
      size = c(40),
      language = c("R"),
      forks_count = c(4),
      url = c("ble"),
      ssh_url = c("more_ble"),
      clone_url = c("even_more_ble"),
      leave_me_out = c("thanks"),
      stringsAsFactors = FALSE)
  }
  list(
    df1 = testdf("testrepo1","2014-04-01 13:09:11"),
    df2 = testdf("testrepo2", "2014-04-02 13:09:11"))
}

test_that("more_repos detects when there is more and when not", {
    expect_false(more_repos("next?page=1"))
    expect_true(more_repos("next?page=2"))
  }
)

test_that("combine_repo_data return expected repo info in expected order", {
    test_dfs <- create_repo_dfs()
    combined <- combine_repo_data(test_dfs)
    expect_false("leave_me_out" %in% names(combined))
    expect_true(combined$updated_at[1] == as.POSIXct("2014-04-02 13:09:11"))
    expect_true(combined$name[1] == "testrepo2")
  }
)

context("getting refined info from repo data")

test_that("repo_download_list extracts download URLs for all repos", {
    combined <- combine_repo_data(create_repo_dfs())
    repo_list <- repo_download_list(combined)
    correct_download_list <- c(
      "ble/commits?page=1?per_page=100",
      "ble/commits?page=1?per_page=100")
    expect_equal(repo_list$download_url, correct_download_list)
  }
)

test_that("repo_name extracts name from URL", {
    test_urls <- c(
      "https://api.github.com/repos/rstudio/rstudio",
      "https://api.github.com/repos/rstudio/ggvis")
    repo_names <- repo_name(test_urls)
    correct_names <- c("rstudio", "ggvis")
    expect_equal(repo_names, correct_names)
  }
)

context("working with commits")

create_test_changes <- function() {
  c("25 files changed, 418 insertions(+), 411 deletions(-)",
    "7 files changed, 72 insertions(+)",
    "1 file changed, 5 deletions(-)")
}

test_that("commit_changes function extracts file changes", {
    changes <- commit_changes(create_test_changes(), "file")
    correct_changes <- c(25, 7, 1)
    expect_equal(changes, correct_changes)
  }
)

test_that("commit_changes function extracts insertions from strings", {
    changes <- commit_changes(create_test_changes(), "insertions")
    correct_changes <- c(418, 72, 0)
    expect_equal(changes, correct_changes)
  }
)

test_that("commit_changes function extracts deletions from strings", {
    changes <- commit_changes(create_test_changes(), "deletions")
    correct_changes <- c(411, 0, 5)
    expect_equal(changes, correct_changes)
  }
)
