library('wrapr')

context("namedMapBuilder")

test_that("test_named_map_builder.R", {
  names <- c("a", "b")
  vals <- c(1,2)
  testthat::expect_equal(names := vals, c(a = 1, b = 2))

  f <- ~x := x*x
  testthat::expect_equal(f(7), 7*7)

  g <- x := { x*x }
  testthat::expect_equal(g(7), 7*7)

  testthat::expect_equal(c("a", "b") := c(1,2), c(a = 1, b = 2))
  testthat::expect_equal("a" := "b", c(a = "b"))

  name <- "a"
  testthat::expect_equal(name := "b", c(a = "b"))
})
