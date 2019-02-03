
test_named_map_builder <- function() {
  names <- c("a", "b")
  vals <- c(1,2)
  RUnit::checkEquals(names := vals, c(a = 1, b = 2))

  f <- ~x := x*x
  RUnit::checkEquals(f(7), 7*7)

  g <- x := { x*x }
  RUnit::checkEquals(g(7), 7*7)

  RUnit::checkEquals(c("a", "b") := c(1,2), c(a = 1, b = 2))
  RUnit::checkEquals("a" := "b", c(a = "b"))

  name <- "a"
  RUnit::checkEquals(name := "b", c(a = "b"))

  invisible(NULL)
}
