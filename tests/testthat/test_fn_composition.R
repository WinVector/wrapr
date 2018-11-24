library('wrapr')

context("function_composition")


test_that("test_fn_composition.R", {

  x <- c(1:5)

  f1 <- new("PartialNamedFn",
           fn_name = "exp",
           fn_package = "base",
           arg_name = "x",
           args = list())


  f2 <- new("PartialFunction",
            fn = base::sin,
            arg_name = "x",
            args = list())

  f3 <- new("UnaryFnList", items = list(f1, f2))

  f4 <- f1 %.>% f2

  v1 <- (x %.>% f1) %.>% f2
  v2 <- x %.>% f1 %.>% f2
  testthat::expect_equal(v1, v2)

  f12 <- f1 %.>% f2
  v3 <- x %.>% f12
  testthat::expect_equal(v1, v2)

  fa <- ApplyTo(f2, f1)
  va <- x %.>% fa
  testthat::expect_equal(v1, va)

  v4 <- ApplyTo(f2, ApplyTo(f1, x))
  testthat::expect_equal(v1, v4)

  v5 <- x %.>% f3
  testthat::expect_equal(v1, v5)

  # see composition doesn't have S4 warnings
  tryCatch({
    z <- f1 %.>% f1
    z <- f1 %.>% f2
    z <- f1 %.>% f3
    z <- f2 %.>% f1
    z <- f2 %.>% f2
    z <- f2 %.>% f3
    z <- f3 %.>% f1
    z <- f3 %.>% f2
    z <- f3 %.>% f3
  },
  error = function(...) { stop("saw error")},
  warning = function(...) { stop("saw warning")}
  )

  z <- new("PartialNamedFn",
      fn_name = "exp",
      fn_package = "base",
      arg_name = "x",
      args = list()) %.>%
    new("PartialFunction",
        fn = base::sin,
        arg_name = "x",
        args = list())
  v <- x %.>% z
})
