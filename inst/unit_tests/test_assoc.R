
test_assoc <- function() {
  x <- 1:3

  f <- wrapfn(sin, "x")
  v1 <- x %.>% f
  RUnit::checkEquals(v1, sin(x))

  invisible(NULL)
}