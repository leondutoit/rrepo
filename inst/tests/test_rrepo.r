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


