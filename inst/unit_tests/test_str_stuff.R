
test_str_stuff <- function() {

  v0 <- split_at_brace_pairs("{x} + y + {z}")
  RUnit::checkEquals(
    v0,
    c("{x}", " + y + ", "{z}"))

  v1 <- strsplit_capture("x is .(x) and x+1 is .(x+1)", "\\.\\([^)]+\\)")

  x <- 7
  v2 <- sinterp("x is .(x), x+1 is .(x+1)\n.(x) is odd is .(x%%2 == 1)")
  RUnit::checkEquals(
    v2,
    c("x is 7, x+1 is 8\n7 is odd is TRUE"))
  v3 <- sinterp("x is .(x), x+1 is .(x+1)\n.(x) is odd is .(x%%2 == 1)",
                envir = list(x = 10))
  RUnit::checkEquals(
    v3,
    c("x is 10, x+1 is 11\n10 is odd is FALSE"))

  d <- data.frame(x = 1:2, y = 3:4)
  v4 <- sinterp("x is .(x), and y is .(y)", envir = d)
  RUnit::checkEquals(
    v4,
    c("x is 1, and y is 3", "x is 2, and y is 4"))

  invisible(NULL)
}

