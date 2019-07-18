
test_slots <- function() {
  # from help("slot")
  setClass("track", slots = c(x="numeric", y="numeric"))
  myTrack <- new("track", x = -4:4, y = exp(-4:4))


  expect <- myTrack@x
  v1 <- myTrack %.>% .@x
  RUnit::checkEquals(expect, v1)

  let(
    c(X = 'x'),
    v2 <- myTrack@X
  )
  RUnit::checkEquals(expect, v2)

  invisible(NULL)
}

