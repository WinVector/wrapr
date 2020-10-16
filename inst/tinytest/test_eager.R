
test_eager <- function() {

  lst <- list(sin)
  attr(lst, 'dotpipe_eager_eval_bracket') <- TRUE
  res <- 4 %.>% lst[[1]]
  expect_equal(sin(4), res)

  f <- function() { sin }
  attr(f, 'dotpipe_eager_eval_function') <- TRUE
  res2 <- 4 %.>% f()
  expect_equal(sin(4), res2)

  invisible(NULL)
}

test_eager()