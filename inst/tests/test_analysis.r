library(testthat)

create_repo_dfs <- function() {
  testdf <- function(name, updated_at, lang, size) {
    data.frame(
      name = c(name),
      description = c("test repo"),
      created_at = c(as.POSIXct("2014-04-01 13:09:11")),
      updated_at = c(as.POSIXct(updated_at)),
      pushed_at = c(as.POSIXct("2014-03-01 13:09:11")),
      size = c(size),
      language = c(lang),
      forks_count = c(4),
      url = c("ble"),
      ssh_url = c("more_ble"),
      clone_url = c("even_more_ble"),
      leave_me_out = c("thanks"),
      stringsAsFactors = FALSE)
  }
  rbind_all(
    list(
      df1 = testdf("testrepo1","2014-04-01 13:09:11", "R", 40),
      df2 = testdf("testrepo2", "2014-04-02 13:09:11", "C", 100)))
}

create_commits <- function() {
  data.frame(
    sha = c(
      "96ace0e79e72631b649f2a901a7473c018f35bf3",
      "43b68ca416dfa218ac2140eb7329528f9098f097",
      "5877688c4733cb936b1d5b43d042d3586c6c6722",
      "cb097bfac5d6f00a7f833175722da8708c9f5631"),
    author = c(
      "Leon du Toit",
      "Line Simenstad",
      "Line Simenstad",
      "Leon du Toit"),
    email = c(
      "ble@joumase.com",
      "bla@motbydelighetsryket.com",
      "bla@motbydelighetsryket.com",
      "ble@joumase.com"),
    date = as.POSIXct(c(
      "2014-04-02 09:39:38 -0400",
      "2014-04-02 12:18:38 -0400",
      "2014-02-28 10:39:38 -0400",
      "2014-02-23 09:00:38 -0400")),
    message = c(
      "hello",
      "there",
      "person",
      "coder"),
    changes = c(
      "25 files changed, 418 insertions(+), 411 deletions(-)",
      "11 files changed, 551 insertions(+)",
      "5 files changed, 3 insertions(+), 485 deletions(-)",
      "5 files changed, 8194 insertions(+)"),
    repo_name = c(
      "jabra",
      "howzitmyboy",
      "howzitmyboy",
      "howzitmyboy"),
    file_changes = c(25, 11, 5, 5),
    insertions = c(418, 551, 3, 8194),
    deletions= c(411, 0, 485, 0),
    stringsAsFactors = FALSE)
}

context("test analysis functions")

test_that("repo_language spits out the correct data", {
    testdata <- create_repo_dfs()
    languages <- repo_language(testdata)
    correct_languages <- data.frame(
      language = c("C", "R"),
      size = c(100, 40),
      stringsAsFactors = FALSE)
    expect_equal(dim(languages), dim(correct_languages))
    expect_equal(languages$language, correct_languages$language)
    expect_equal(languages$size, correct_languages$size)
  }
)

test_that("commits_per_week aggregates correctly", {
    test_commits <- create_commits()
    per_week <- commits_per_week(test_commits)
    correct_per_week <- data.frame(
      weekdate = as.Date(c("2014-02-23", "2014-03-30")),
      commits = c(2, 2),
      stringsAsFactors = FALSE)
    expect_equal(per_week$weekdate, correct_per_week$weekdate)
    expect_equal(per_week$commits, correct_per_week$commits)
  }
)

test_that("projects_over_time aggregates correctly", {
    test_commits <- create_commits()
    over_time <- projects_over_time(test_commits)
    correct_over_time <- data.frame(
      repo_name = c("howzitmyboy", "howzitmyboy", "jabra"),
      date = as.Date(c( "2014-02-23", "2014-03-30", "2014-03-30")),
      commits = c(2, 1, 1),
      max_commits = c(2, 2, 1),
      percentage = c(10, 5, 10),
      stringsAsFactors = FALSE)
    expect_equal(dim(over_time), dim(correct_over_time))
    expect_equal(over_time$repo_name, correct_over_time$repo_name)
    expect_equal(over_time$date, correct_over_time$date)
    expect_equal(over_time$commits, correct_over_time$commits)
    expect_equal(over_time$max_commits, correct_over_time$max_commits)
    expect_equal(over_time$percentage, correct_over_time$percentage)
  }
)

test_that("author_contributions returns what it should", {
    test_commits <- create_commits()
    contributions <- author_contributions(test_commits)
    correct_contributions <- data.frame(
      author = c("Leon du Toit", "Line Simenstad"),
      files_contrib = c(30, 16),
      insertions_contrib = c(8612, 554),
      deletions_contrib = c(411, 485),
      stringsAsFactors = FALSE)
    expect_equal(dim(contributions), dim(correct_contributions))
    expect_equal(contributions$author, correct_contributions$author)
    expect_equal(contributions$files_contrib, correct_contributions$files_contrib)
    expect_equal(contributions$insertions_contrib, correct_contributions$insertions_contrib)
    expect_equal(contributions$deletions_contrib, correct_contributions$deletions_contrib)
  }
)
