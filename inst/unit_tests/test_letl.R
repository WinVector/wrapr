
test_letl <- function() {

  d <- data.frame(
    year= c(2005, 2005),
    name= c('a', 'a'),
    stringsAsFactors = FALSE)

  dstr <- d
  let(list(NEWCOL='val'),
      dstr$NEWCOL <- 7,
      subsMethod= 'stringsubs'
  )
  dlan <- d
  let(list(NEWCOL='val'),
      dlan$NEWCOL <- 7,
      subsMethod= 'langsubs'
  )
  #RUnit::checkEquals(dsub, dstr)
  RUnit::checkEquals(dstr, dlan)

  invisible(NULL)
}