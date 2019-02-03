
test_qae <- function() {
  v <- wrapr::qae(a = 1, b := 2, c %:=% 3)
  RUnit::checkEquals(c(a = "1", b = "2", c = "3"), v)

  v2 <- qae(a = 1, b := 2, c %:=% 3)
  RUnit::checkEquals(c(a = "1", b = "2", c = "3"), v2)

  invisible(NULL)
}
