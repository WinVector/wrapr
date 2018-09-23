
context("qae")

test_that("test_qae.R", {
  v <- wrapr::qae(a = 1, b := 2, c %:=% 3)
  expect_equal(c(a = "1", b = "2", c = "3"), v)
  library("wrapr")
  v2 <- qae(a = 1, b := 2, c %:=% 3)
  expect_equal(c(a = "1", b = "2", c = "3"), v2)
})
