
test_strict <- function() {
  # some strictness from wrapr pipe
  expect_error(
    5 %.>% sin()
  )

  badf <- function(x) {
    x %.>% return(.)
    return(7)
  }
  expect_error(
    badf(7)
  )

  expect_equal(sin(5), 5 %.>% sin(.))

  expect_equal(sin(5), 5 %.>% (sin(.)))

  expect_equal(sin(5), 5 %.>% {sin(.)})

  invisible(NULL)
}

test_strict()

