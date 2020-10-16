

test_fn_test <- function() {
  # make sure we see functions, even if they have other classes
  f <- function(x) {
    sin(x)
  }

  r1 <- 5 %.>% f
  expect_equal(sin(5), r1)

  class(f) <- "SOME_ODD_NAME"
  r2 <- 5 %.>% f
  expect_equal(sin(5), r2)

  invisible(NULL)
}

test_fn_test()
