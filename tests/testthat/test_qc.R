library('wrapr')

context("qc")

test_that("test_qc.R", {

  a <- "x"
  expect_equal(qc(a), "a")

  expect_equal(qc("a"), "a")

  expect_equal(qc(sin(x)), "sin(x)")

  expect_equal(qc(a, qc(b, c)), c("a", "b", "c"))

  expect_equal(qc(x=a, qc(y=b, z=c)), c(x="a", y="b", z="c"))

  expect_equal(qc('x'='a', wrapr::qc('y'='b', 'z'='c')), c(x="a", y="b", z="c"))

})
