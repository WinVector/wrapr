
test_split_braces <- function() {
  expect_equal(
    split_at_brace_pairs("{x} + y"),
    c("{x}", " + y"))

  expect_equal(
    split_at_brace_pairs("{x} + y + {z}"),
    c("{x}", " + y + ", "{z}"))

  expect_equal(
    split_at_brace_pairs(list("{x} + y + {z}")),
    list(c("{x}", " + y + ", "{z}")))


  expect_equal(
    split_at_brace_pairs("x + {y} + z"),
    c("x + ", "{y}", " + z"))

  expect_equal(
    split_at_brace_pairs("x + y"),
    "x + y")

  expect_equal(
    split_at_brace_pairs(""),
    "")

  expect_equal(
    split_at_brace_pairs(c("{x} + y",
                           "{x} + y + {z}")),
    list(c("{x}", " + y"),
         c("{x}", " + y + ", "{z}")))

  expect_equal(
    split_at_brace_pairs(list("{x} + y",
                              "{x} + y + {z}")),
    list(c("{x}", " + y"),
         c("{x}", " + y + ", "{z}")))

  expect_equal(
    split_at_brace_pairs("-<hi>- <hi> -hi-", open_symbol = "-<", close_symbol = ">-"),
    c("-<hi>-", " <hi> -hi-"))

  expect_equal(
    split_at_brace_pairs("x + .[y]+z", open_symbol = ".[", close_symbol = "]"),
    c("x + ", ".[y]", "+z"))

  expect_equal(
    wrapr::split_at_brace_pairs(list(nm = "sqrt(.[v1])"), open_symbol = ".[", close_symbol = "]"),
    list(nm = c("sqrt(", ".[v1]", ")" )))

  invisible(NULL)
}

test_split_braces()

