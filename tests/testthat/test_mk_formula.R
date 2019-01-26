library('wrapr')

context("formula_construction")


test_that("test_mk_formula.R", {

  v <- format(wrapr:::r_plus(c(), add_zero = FALSE))
  testthat::expect_equal("1", v)

  v <- format(wrapr:::r_plus(c(), add_zero = TRUE))
  testthat::expect_equal("0", v)

  v <- as.character(wrapr:::r_plus(c('x1'), add_zero = FALSE))
  testthat::expect_equal("x1", v)

  v <- format(wrapr:::r_plus(c('x1'), add_zero = TRUE))
  testthat::expect_equal("0 + x1", v)

  v <- format(wrapr:::r_plus(c('x1', 'x2'), add_zero = FALSE))
  testthat::expect_equal("x1 + x2", v)

  v <- format(wrapr:::r_plus(c('x1', 'x2'), add_zero = TRUE))
  testthat::expect_equal("0 + x1 + x2", v)

  v <- format(mk_formula("y", c(), intercept = FALSE))
  testthat::expect_equal("y ~ 0", v)

  v <- format(mk_formula("y", c(), intercept = TRUE))
  testthat::expect_equal("y ~ 1", v)

  v <- format(mk_formula("y", c('x1'), intercept = TRUE))
  testthat::expect_equal("y ~ x1", v)

  v <- format(mk_formula("y", c('x1'), intercept = FALSE))
  testthat::expect_equal("y ~ 0 + x1", v)

  v <- format(mk_formula("y", c('x1', 'x2'), intercept = TRUE))
  testthat::expect_equal("y ~ x1 + x2", v)

  v <- format(mk_formula("y", c('x1', 'x2'), intercept = FALSE))
  testthat::expect_equal("y ~ 0 + x1 + x2", v)


  f <- mk_formula("mpg", c("cyl", "disp"))
  # print(f)
  model <- lm(f, mtcars)
  v <- format(model$terms)
  testthat::expect_equal("mpg ~ cyl + disp", v)

  f <- mk_formula("cyl", c("wt", "gear"), outcome_target = 8, outcome_comparator = ">=")
  # print(f)
  model <- glm(f, mtcars, family = binomial)
  v <- format(model$terms)
  testthat::expect_equal("(cyl >= 8) ~ wt + gear", v)

})
