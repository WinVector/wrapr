library('wrapr')

context("split_brace")

test_that("test_split_braces.R", {

  testthat::expect_equal(split_at_brace_pairs("{x} + y"),
                         c("{x}", " + y"))

  testthat::expect_equal(split_at_brace_pairs("{x} + y + {z}"),
                         c("{x}", " + y + ", "{z}"))

  testthat::expect_equal(split_at_brace_pairs("x + {y} + z"),
                         c("x + ", "{y}", " + z"))

  testthat::expect_equal(split_at_brace_pairs("x + y"),
                         "x + y")

  testthat::expect_equal(split_at_brace_pairs(""),
                         "")

  testthat::expect_equal(split_at_brace_pairs(c("{x} + y",
                                                "{x} + y + {z}")),
                                              list(c("{x}", " + y"),
                                                   c("{x}", " + y + ", "{z}")))

})
