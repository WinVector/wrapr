test_unpack <- function() {

  a <- 'x'
  # name capture version
  into[a, b] <- list(5, 10)
  RUnit::checkEquals(a, 5)
  RUnit::checkEquals(b, 10)

  # into is now in local namespace, 2nd check is also checking that still works

  # bquote re-direct to value in variable
  # plus quotes are allowed
  a <- 'x'
  into[.(a), 'b'] <- list(20, 40)
  RUnit::checkEquals(a, 'x')
  RUnit::checkEquals(x, 20)
  RUnit::checkEquals(b, 40)

  list(56, 106) %.>% into[a, b]
  RUnit::checkEquals(a, 56)
  RUnit::checkEquals(b, 106)

  list(561, 1061) %.>% .(into[a, b])
  RUnit::checkEquals(a, 561)
  RUnit::checkEquals(b, 1061)

  list(256, 2106) %.>% into(a, b)
  RUnit::checkEquals(a, 256)
  RUnit::checkEquals(b, 2106)

  list(2561, 21061) %.>% .(into(a, b))
  RUnit::checkEquals(a, 2561)
  RUnit::checkEquals(b, 21061)

  list(7, 12) %.>% unpack_to(., a, b)
  RUnit::checkEquals(a, 7)
  RUnit::checkEquals(b, 12)

  invisible(NULL)
}
