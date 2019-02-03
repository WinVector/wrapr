
test_build_frame <- function() {
  testBFRT <- function(d) {
    txt <- draw_frame(d)
    d2 <- eval(parse(text = txt))
    RUnit::checkEquals(d, d2)
  }

  d <- data.frame(
    measure = c("minus binary cross entropy", "accuracy"),
    training = c(5, 0.8),
    validation = c(-7, 0.6),
    stringsAsFactors = FALSE)
  testBFRT(d)

  d <- data.frame(x = c(-1, 2))
  testBFRT(d)

  d <- data.frame(x = 1)
  testBFRT(d)

  d <- data.frame(x = 1L)
  testBFRT(d)

  d1 <- qchar_frame(
    measure,                      training, validation |
    "minus binary cross entropy", loss,     val_loss   |
    accuracy,                     acc,      val_acc    )
  d2 <- data.frame(
    measure = c("minus binary cross entropy", "accuracy"),
    training = c("loss", "acc"),
    validation = c("val_loss", "val_acc"),
    stringsAsFactors = FALSE)
  RUnit::checkEquals(d1, d2)

  d1 <- qchar_frame(
    x |
    1 |
    2 )
  d2 <- data.frame(
    x = c("1", "2"),
    stringsAsFactors = FALSE)
  RUnit::checkEquals(d1, d2)

  d1 <- data.frame(
    idx = c(1L, 2L),
    time = strptime(c("02/27/92 23:03:20", "02/27/92 22:29:56"),
                    "%m/%d/%y %H:%M:%S"),
    val = c(4, 10),
    lab = c("a", "b"),
    stringsAsFactors = FALSE)
  txt <- draw_frame(d1)

  invisible(NULL)
}
