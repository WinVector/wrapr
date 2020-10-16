

test_dot_quote <- function() {

  # # can't work due to the nature of bquote()
  # a <- wrapr::qchar_frame(
  #   "."             , "pred: FALSE", "pred: TRUE" |
  #     "truth: FALSE",          tFpF,          tFpT ,
  #     "truth: TRUE" ,          tTpF,          tTpT)
  #
  # b <- wrapr::build_frame(
  #   "."             , "pred: FALSE", "pred: TRUE" |
  #     "truth: FALSE", "tFpF"       , "tFpT"       |
  #     "truth: TRUE" , "tTpF"       , "tTpT"       )
  #
  # expect_equal(a, b)

  c <- wrapr::qchar_frame(
    TRUE  , FALSE |
      TRUE, FALSE )

  d <- wrapr::build_frame(
    "TRUE"  , "FALSE" |
      "TRUE", "FALSE" )

  expect_equal(c, d)

  invisible(NULL)
}

test_dot_quote()

