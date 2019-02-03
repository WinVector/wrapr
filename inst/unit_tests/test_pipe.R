
test_pipe <- function() {
  # adapted from
  # library("magrittr")
  # library("wrapr")
  #
  # mtcars %>%
  #   subset(hp > 100) %>%
  #   transform(kpl = mpg %>% multiply_by(0.4251)) %>%
  #   aggregate(. ~ cyl, data = ., FUN = . %>% mean %>% round(2)) %>%
  #   draw_frame(., formatC_options = list(format = "f", digits = 2)) %>%
  #   cat(.)
  expect <- build_frame(
    "cyl", "mpg", "disp", "hp"  , "drat", "wt", "qsec", "vs", "am", "gear", "carb", "kpl" |
    4.00 , 25.90, 108.05, 111.00, 3.94  , 2.15, 17.75 , 1.00, 1.00, 4.50  , 2.00  , 11.01 |
    6.00 , 19.74, 183.31, 122.29, 3.59  , 3.12, 17.98 , 0.57, 0.43, 3.86  , 3.43  , 8.39  |
    8.00 , 15.10, 353.10, 209.21, 3.23  , 4.00, 16.77 , 0.00, 0.14, 3.29  , 3.50  , 6.42  )

  res <-
    mtcars %.>%
    subset(., hp > 100) %.>%
    transform(., kpl = mpg * 0.4251) %.>%
    aggregate(. ~ cyl, data = ., FUN = . := { mean(.) %.>% round(., 2) })

  RUnit::checkEquals(expect, res)

  lst <- list(h = sin)
  res <- 5 %.>% lst$h
  RUnit::checkEquals(sin(5), res)
  res <- 5 %.>% lst[['h']]
  RUnit::checkEquals(sin(5), res)

  invisible(NULL)
}
