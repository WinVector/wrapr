
test_split_braces <- function() {
  RUnit::checkEquals(
    split_at_brace_pairs("{x} + y"),
    c("{x}", " + y"))

  RUnit::checkEquals(
    split_at_brace_pairs("{x} + y + {z}"),
    c("{x}", " + y + ", "{z}"))

  RUnit::checkEquals(
    split_at_brace_pairs(list("{x} + y + {z}")),
    list(c("{x}", " + y + ", "{z}")))


  RUnit::checkEquals(
    split_at_brace_pairs("x + {y} + z"),
    c("x + ", "{y}", " + z"))

  RUnit::checkEquals(
    split_at_brace_pairs("x + y"),
    "x + y")

  RUnit::checkEquals(
    split_at_brace_pairs(""),
    "")

  RUnit::checkEquals(
    split_at_brace_pairs(c("{x} + y",
                           "{x} + y + {z}")),
    list(c("{x}", " + y"),
         c("{x}", " + y + ", "{z}")))

  RUnit::checkEquals(
    split_at_brace_pairs(list("{x} + y",
                              "{x} + y + {z}")),
    list(c("{x}", " + y"),
         c("{x}", " + y + ", "{z}")))

  RUnit::checkEquals(
    split_at_brace_pairs("-<hi>- <hi> -hi-", open_symbol = "-<", close_symbol = ">-"),
    c("-<hi>-", " <hi> -hi-"))

  RUnit::checkEquals(
    split_at_brace_pairs("x + .[y]+z", open_symbol = ".[", close_symbol = "]"),
    c("x + ", ".[y]", "+z"))

  RUnit::checkEquals(
    wrapr::split_at_brace_pairs(list(nm = "sqrt(.[v1])"), open_symbol = ".[", close_symbol = "]"),
    list(nm = c("sqrt(", ".[v1]", ")" )))

  invisible(NULL)
}
