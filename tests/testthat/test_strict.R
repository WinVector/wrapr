library('wrapr')

context("strictness")


test_that("test_strict.R", {
  # some strictness from wrapr pipe
  expect_error(
    5 %.>% sin()
  )

  badf <- function(x) {
    x %.>% return(.)
    return(7)
  }
  expect_error(
    badf(7)
  )

  expect_equal(sin(5), 5 %.>% sin(.))

  expect_equal(sin(5), 5 %.>% (sin(.)))

  expect_equal(sin(5), 5 %.>% {sin(.)})

})