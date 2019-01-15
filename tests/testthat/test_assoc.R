library('wrapr')

context("test_assoc")

test_that("test_assoc.R", {
  x <- 1:3

  f <- wrapfn(sin, "x")
  v1 <- x %.>% f
  testthat::expect_equivalent(v1, sin(x))
})