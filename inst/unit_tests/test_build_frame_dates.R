
test_build_frame_dates <- function() {
  d1 <- wrapr::build_frame(
    "date", "measure", "value" |
      as.Date("2019-03-01")   , "AUC"    , as.Date("2019-03-11")     |
      as.Date("2019-03-01")   , "R2"     , as.Date("2019-03-12")     |
      as.Date("2019-03-02")   , "AUC"    , as.Date("2019-03-13")     |
      as.Date("2019-03-02")   , "R2"     , as.Date("2019-03-14")     )

  d <- data.frame(date = c(as.Date("2019-03-01") , as.Date("2019-03-01"), as.Date("2019-03-02") , as.Date("2019-03-02") ),
                  measure = c("AUC", "R2", "AUC", "R2"),
                  value = c(as.Date("2019-03-11") , as.Date("2019-03-12"), as.Date("2019-03-13") , as.Date("2019-03-14") ),
                  stringsAsFactors = FALSE)
  RUnit::checkTrue("Date" %in% class(d1$date))
  RUnit::checkTrue("Date" %in% class(d1$value))
  RUnit::checkTrue(wrapr::check_equiv_frames(d1, d))

  invisible(NULL)
}
