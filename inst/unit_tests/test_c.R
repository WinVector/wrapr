

test_c <- function() {
  ` X` <- 3
  y <- 7
  X <- 2
  X_ <- 5

  let(
    c(X = 'y', F = 'sin'),
    {
      d <- data.frame("X" = "X", X2 = "XX", d = X*X, .X = X_,
                      stringsAsFactors = FALSE)
      X <- list(X = d$X, X2 = d$"X", v1 = `X`, v2 = ` X`, fX = F(1:3))
    })
  RUnit::checkEquals(y$X2, 'X')

  invisible(NULL)
}

