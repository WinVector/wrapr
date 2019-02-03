
test_coalesce <- function() {
  RUnit::checkEquals(c(1, NA, NA) %?% 5             , c(1, 5, 5))
  RUnit::checkEquals(c(1, NA, NA) %?% list(5)       , c(1, 5, 5))
  RUnit::checkEquals(c(1, NA, NA) %?% list(list(5)) , c(1, NA, NA))
  RUnit::checkEquals(c(1, NA, NA) %?% c(NA, 20, NA) , c(1, 20, NA))
  RUnit::checkEquals(NULL %?% list()    , NULL)
  RUnit::checkEquals(NULL %?% c(1, NA) , c(1, NA))
  RUnit::checkEquals(list(1, NULL, NULL) %?% c(3, 4, NA)                         , list(1, 4, NA_real_))
  RUnit::checkEquals(list(1, NULL, NULL, NA, NA) %?% list(2, NULL, NA, NULL, NA) , list(1, NULL, NA, NULL, NA))
  RUnit::checkEquals(c(1, NA, NA) %?% list(1, 2, list(3)) , c(1, 2, NA))
  RUnit::checkEquals(c(1, NA) %?% list(1, NULL)           , c(1, NA))
  RUnit::checkEquals(list() %?% list(1, NA, NULL), list(1, NA, NULL) )
  RUnit::checkEquals(c() %?% list(1, NA, NULL)    , list(1, NA, NULL) )
  RUnit::checkEquals(c() %?% c(1, NA, 2)          , c(1, NA, 2))
  RUnit::checkEquals(c(a=1, b= NA) %?% list(3,4), c(a=1, b=4))
  RUnit::checkEquals(list(a=1, b= NA) %?% c(3,4), list(a=1, b=4))
  RUnit::checkEquals(NULL %?% 4, 4)

  invisible(NULL)
}
