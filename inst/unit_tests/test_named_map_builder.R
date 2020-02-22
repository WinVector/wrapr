
test_named_map_builder <- function() {
  names <- c("a", "b")
  vals <- c(1,2)
  RUnit::checkEquals(names := vals, c(a = 1, b = 2))

  RUnit::checkEquals(c("a", "b") := c(1,2), c(a = 1, b = 2))
  RUnit::checkEquals("a" := "b", c(a = "b"))

  name <- "a"
  RUnit::checkEquals(name := "b", c(a = "b"))

  f <- factor(c('a', 'b'))
  RUnit::checkEquals(f := c(1, 2), c('a' = 1, 'b' = 2))

  invisible(NULL)
}
