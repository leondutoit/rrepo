library(testthat)

create_test_response <- function(any_left = TRUE) {
  if (any_left) {
    header <- list(
        link = paste(
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
    headers = header)
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

context("repo analysis")

commit_file_to_repo <- function(repo, path, filename) {
  writeLines("some data for the file", file.path(path, filename))
  add(repo, filename)
  commit(repo, "Commit message")
}

create_test_repo <- function() {
  path <- file.path(tempfile("test-repo-"))
  print(paste("created test repo at ", path))
  dir.create(path)
  repo <- init(path)
  commit_file_to_repo(repo, path, "file1")
  commit_file_to_repo(repo, path, "file2")
  repository(path)
}

remove_test_repo <- function(repo) {
  system(paste("rm -rf"), slot(repo, "path"),
    intern = T, ignore.stdout = T, ignore.stderr = T, wait = F)
}

test_that("git_log_to_df extracts all commit data", {
  test_repo <- create_test_repo()
  commit_log <- git_log_to_df(test_repo)
  expect_equal(names(commit_log), c("sha", "name", "email", "date", "message", "repo"))
  expect_equal(length(commit_log$sha), 2)
  remove_test_repo(test_repo)
})

test_that("get_all_commit_data does so", {
  test_repo1 <- create_test_repo()
  test_repo2 <- create_test_repo()
  test_repos <- list(test_repo1, test_repo2)
  commit_log <- get_all_commit_data(test_repos)
  expect_equal(names(commit_log), c("sha", "name", "email", "date", "message", "repo"))
  expect_equal(length(commit_log$sha), 4)
  remove_test_repo(test_repo1)
  remove_test_repo(test_repo2)
})
