
test_named_map_builder <- function() {
  names <- c("a", "b")
  vals <- c(1,2)
  expect_equal(names := vals, c(a = 1, b = 2))

  expect_equal(c("a", "b") := c(1,2), c(a = 1, b = 2))
  expect_equal("a" := "b", c(a = "b"))

  name <- "a"
  expect_equal(name := "b", c(a = "b"))

  f <- factor(c('a', 'b'))
  expect_equal(f := c(1, 2), c('a' = 1, 'b' = 2))

  invisible(NULL)
}

test_named_map_builder()

