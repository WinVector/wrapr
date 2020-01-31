
test_as_named_list <- function() {

  a <- data.frame(x = 1)
  b <- 2

  l0 <- as_named_list(a)
  expect0 <- list(a = a)
  RUnit::checkEquals(l0, expect0)

  l1 <- as_named_list(a, b)
  expect1 <- list(a = a, b = b)
  RUnit::checkEquals(l1, expect1)

  l2 <- as_named_list(a, x = b, c = 1 + 1, d = NULL)
  expect2 <- list(a = a, x = b, c = 2, d = NULL)
  RUnit::checkEquals(l2, expect2)

  RUnit::checkException(as_named_list(a, a), silent = TRUE)

  RUnit::checkException(as_named_list(NULL), silent = TRUE)

  RUnit::checkException(as_named_list(a, NULL), silent = TRUE)

  RUnit::checkException(as_named_list(7), silent = TRUE)

  invisible(NULL)
}