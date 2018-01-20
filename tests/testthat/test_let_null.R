library('wrapr')

context("letnull")

test_that("test_let_null.R", {
  e1 = as.character(let(c("z"= "z"), dDT[,"x":=NULL], eval=FALSE))
  e2 = as.character(let(list(), dDT[,"x":=NULL], eval=FALSE))
  testthat::expect_equal(e1, e2)
})