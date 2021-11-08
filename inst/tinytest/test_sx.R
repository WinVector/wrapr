

expect_equal(
  sx('1 2 "c", d'), c("1", "2", "c", "d")
)

expect_equal(
  sx('1 2 3'), c("1", "2", "3")
)

expect_equal(
  sx('1 2 "3"'), c("1", "2", "3")
)

expect_equal(
  sx('1,2|3.4'), c("1", "2", "3.4")
)

expect_equal(
  sx('01 02'), c("01", "02")
)

expect_equal(
  sx('0x3 0z3'), c("0x3", "0z3")
)

