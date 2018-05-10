library('wrapr')

context("draw_frame")

test_that("test_draw_frame.R", {

  ex <- data.frame(id = 1:3,
                   x = c(0, 1, NA),
                   res = c("not one", "one", NA),
                   stringsAsFactors = FALSE)
  f <- build_frame(
    "id", "x", "res"     |
    1L  , 0  , "not one" |
    2L  , 1  , "one"     |
    3L  , NA , NA        )
  expect_equal(ex, f)

  df <- draw_frame(ex)
  f2 <- eval(parse(text = df))
  expect_equal(ex, f2)
})
