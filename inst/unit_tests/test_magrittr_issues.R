
# Get some diversity of examples by using
# issues submitted to magrittr

test_magrittr_issues <- function() {
  # https://github.com/tidyverse/magrittr/issues/159
  # note: magrittr works of compose() forces arguments
  compose <- function(f, g) {
    # force(f)
    # force(g)
    function(x) g(f(x))
  }
  plus1 <- function(x) x + 1
  plus2 <- plus1 %.>% compose(plus1, .)
  res <- plus2(5)
  RUnit::checkEquals(7, res)

  # https://github.com/tidyverse/magrittr/issues/38
  8 %.>% assign("x", .)
  RUnit::checkEquals(8, x)

  # similar to
  # https://github.com/tidyverse/magrittr/issues/105
  res <- 9 %.>% base::sin
  RUnit::checkEquals(sin(9), res)


  # https://github.com/tidyverse/magrittr/issues/156
  # not an issue for magritter with dot notations
  flist <- list(f = sin)
  res <- 5 %.>% flist$f
  RUnit::checkEquals(sin(5), res)
  res <- 5 %.>% flist[['f']]
  RUnit::checkEquals(sin(5), res)


  # https://github.com/tidyverse/magrittr/issues/32
  RUnit::checkException(
    5 %.>% return(.)
  )

  # https://github.com/tidyverse/magrittr/issues/163
  # wrapr doesn't comment to fully gauranteeing this, but nice to confirm
  rm(list=ls())
  global <- 1
  f <- function(x, env = parent.frame()) {
    ls(env)
  }
  v1 <- NA
  v2 <- NA
  v1 <- f(1)
  v2 <- 1 %.>% f
  RUnit::checkEquals(v1, v2)

  # https://github.com/tidyverse/magrittr/issues/121
  res <- 1:3 %.>% .[-1][-1]
  RUnit::checkEquals(3, res)

  # # . getting captured in various environments
  # # wrapr is more explicit with . so has more of these dangling refs
  # https://github.com/tidyverse/magrittr/issues/146

  invisible(NULL)
}
