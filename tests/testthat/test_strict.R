library('wrapr')

context("strictness")


test_that("test_strict.R", {
  # some strictness from wrapr pipe
  expect_error(
    5 %.>% sin()
  )

  expect_error(
    5 %.>% 5
  )

  expect_error(
    5 %.>% return(.)
  )


})