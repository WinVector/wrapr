
expect_equal(
  bc('1 2 "c", d'),
  c("1", "2", "c", "d"))

expect_equal(
  bc('1 2 3'),
  c(1, 2, 3)
)

expect_equal(
  bc('1 2 "3"'),
  c("1", "2", "3")
)

expect_equal(
  bc('1,2|3.4'),
  c(1, 2, 3.4)
)


expect_equal(
  bc('0xF7 10'),
  c(247, 10)
)

expect_equal(
  bc(''),
  NULL
)

expect_equal(
  bc('0x7,0x7'),
  c(7, 7)
)

expect_error(
  bc('0x70x7')
)

expect_equal(
  bc("'x\\''"),
  "x\\'"
)

expect_error(
  bc("'x''")
)


expect_equal(
  bc("TRUE FALSE"),
  c(TRUE, FALSE)
)

expect_error(
  bc("'x' y 7 + 3")
)

expect_equal(
  bc('"a|b" "c,d",f "g e"|q,"t\\"z"'),
  c("a|b", "c,d", "f", "g e", "q", "t\\\"z")
)

expect_equal(
  bc('"a|b"    "c,d"    "f"      "g e"    "q"      "t\\\"z"'),
    c("a|b",   "c,d",   "f",     "g e",   "q",     "t\\\"z")
)
