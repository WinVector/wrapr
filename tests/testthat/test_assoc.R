library('wrapr')

context("test_assoc")

test_that("test_assoc.R", {

  x <- 1:3

  f <- wrapfn(sin, "x")
  x %.>% f

  # note:
  #  x %.>% wrapfn(sin, "x")
  # doesn't use x
  # as we are in the apply-left code path
  # can we have an "as_argument" class that forces apply_right?
  x %.>% wrapfn(sin, "x")
})