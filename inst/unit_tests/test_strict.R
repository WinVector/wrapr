
test_strict <- function() {
  # some strictness from wrapr pipe
  RUnit::checkException(
    5 %.>% sin()
  )

  badf <- function(x) {
    x %.>% return(.)
    return(7)
  }
  RUnit::checkException(
    badf(7)
  )

  RUnit::checkEquals(sin(5), 5 %.>% sin(.))

  RUnit::checkEquals(sin(5), 5 %.>% (sin(.)))

  RUnit::checkEquals(sin(5), 5 %.>% {sin(.)})

  invisible(NULL)
}
