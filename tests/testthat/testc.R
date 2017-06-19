library('wrapr')

context("letc")

test_that("testc.R", {
  ` X` <- 3
  y <- 7
  X <- 2

  let(
    c(X = 'y'),
    {
      d <- data.frame("X" = "X", X2 = "XX", d = X*X, .X = X_,
                      stringsAsFactors = FALSE)
      X <- list(X = d$X, X2 = d$"X", v1 = `X`, v2 = ` X`)
    })
  expect_equal(y$X2, 'X')
})
