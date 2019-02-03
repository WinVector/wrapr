
test_qc <- function() {
  a <- "x"

  RUnit::checkEquals(qc(a), "a")

  RUnit::checkEquals(qc(.(a)), "x")

  RUnit::checkEquals(qc(.(a) := a), c("x" = "a"))

  RUnit::checkEquals(qc("a"),  "a")

  RUnit::checkEquals(qc(sin(x)), "sin(x)")

  RUnit::checkEquals(qc(a, qc(b, c)), c("a", "b", "c"))

  RUnit::checkEquals(qc(a, c("b", "c")), c("a", "b", "c"))

  RUnit::checkEquals(qc(x=a, qc(y=b, z=c)), c(x="a", y="b", z="c"))

  RUnit::checkEquals(qc('x'='a', wrapr::qc('y'='b', 'z'='c')), c(x="a", y="b", z="c"))

  #c(a = c(a="1", b="2")) # returns c(a.a = "1", a.b = "2")
  RUnit::checkEquals(qc(a = c(a=1, b=2)), c(a.a = "1", a.b = "2"))

  RUnit::checkEquals(qc(a := c(a=1, b=2)), c(a.a = "1", a.b = "2"))

  RUnit::checkEquals(qc(c("a", "b") := c("d", "e")), c(a = "d", b = "e"))

  RUnit::checkEquals(qc(x = a, qc(b, z = c)), c("x" = "a", qc("b", "z" = "c")))

  RUnit::checkEquals(qc(x := a, qc(b, z := c)), c("x" = "a", qc("b", "z" = "c")))

  invisible(NULL)
}
