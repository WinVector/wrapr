library('wrapr')

context("coalesce")


test_that("test_coalesce.R", {
  expect_equal(c(1, NA, NA) %?% 5             , c(1, 5, 5))
  expect_equal(c(1, NA, NA) %?% list(5)       , c(1, 5, 5))
  expect_equal(c(1, NA, NA) %?% list(list(5)) , c(1, NA, NA))
  expect_equal(c(1, NA, NA) %?% c(NA, 20, NA) , c(1, 20, NA))
  expect_equal(NULL %?% list()    , NULL)
  expect_equal(NULL %?% c(1, NA) , c(1, NA))
  expect_equal(list(1, NULL, NULL) %?% c(3, 4, NA)                         , list(1, 4, NA_real_))
  expect_equal(list(1, NULL, NULL, NA, NA) %?% list(2, NULL, NA, NULL, NA) , list(1, NULL, NA, NULL, NA))
  expect_equal(c(1, NA, NA) %?% list(1, 2, list(3)) , c(1, 2, NA))
  expect_equal(c(1, NA) %?% list(1, NULL)           , c(1, NA))
  expect_error(list() %?% list(1, NA, NULL))
  expect_equal(c() %?% list(1, NA, NULL)    , list(1, NA, NULL) )
  expect_equal(c() %?% c(1, NA, 2)          , c(1, NA, 2))
  expect_equal(c(a=1, b= NA) %?% list(3,4), c(a=1, b=4))
  expect_equal(list(a=1, b= NA) %?% c(3,4), list(a=1, b=4))
})
