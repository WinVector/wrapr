
test_qc <- function() {
  a <- "x"

  expect_equal(qc(a), "a")

  expect_equal(qc(.(a)), "x")

  expect_equal(qc(.(a) := a), c("x" = "a"))

  expect_equal(qc("a"),  "a")

  expect_equal(qc(sin(x)), "sin(x)")

  expect_equal(qc(a, qc(b, c)), c("a", "b", "c"))

  expect_equal(qc(a, c("b", "c")), c("a", "b", "c"))

  expect_equal(qc(x=a, qc(y=b, z=c)), c(x="a", y="b", z="c"))

  expect_equal(qc('x'='a', wrapr::qc('y'='b', 'z'='c')), c(x="a", y="b", z="c"))

  #c(a = c(a="1", b="2")) # returns c(a.a = "1", a.b = "2")
  expect_equal(qc(a = c(a=1, b=2)), c(a.a = "1", a.b = "2"))

  expect_equal(qc(a := c(a=1, b=2)), c(a.a = "1", a.b = "2"))

  expect_equal(qc(c("a", "b") := c("d", "e")), c(a = "d", b = "e"))

  expect_equal(qc(x = a, qc(b, z = c)), c("x" = "a", qc("b", "z" = "c")))

  expect_equal(qc(x := a, qc(b, z := c)), c("x" = "a", qc("b", "z" = "c")))

  invisible(NULL)
}


test_qc()
