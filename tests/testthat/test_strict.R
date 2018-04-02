library('wrapr')

context("strictness")


test_that("test_strict.R", {
  # some strictness from wrapr pipe
  expect_error(
    5 %.>% sin()
  )

  # not useful, as the function is not applied.
  # but compatible with "turning checks off documentation
  gen <- function(...) { sin }
  expect_error(
    5 %.>% gen()
  )
  expect_error(
    5 %.>% (gen())
  )
  5 %.>% {gen()}

  expect_error(
    5 %.>% 5
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