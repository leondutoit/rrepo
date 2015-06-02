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
