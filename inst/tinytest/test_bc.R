
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
  bc('w9'),
  'w9'
)

