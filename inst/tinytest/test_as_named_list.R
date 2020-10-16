
test_as_named_list <- function() {

  a <- data.frame(x = 1)
  b <- 2

  l0 <- as_named_list(a)
  expect0 <- list(a = a)
  expect_equal(l0, expect0)

  l1 <- as_named_list(a, b)
  expect1 <- list(a = a, b = b)
  expect_equal(l1, expect1)

  l2 <- as_named_list(a, x = b, c = 1 + 1, d = NULL)
  expect2 <- list(a = a, x = b, c = 2, d = NULL)
  expect_equal(l2, expect2)

  expect_error(as_named_list(a, a))

  expect_error(as_named_list(NULL))

  expect_error(as_named_list(a, NULL))

  expect_error(as_named_list(7))

  invisible(NULL)
}

test_as_named_list()

