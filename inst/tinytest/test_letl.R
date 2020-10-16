
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
  #expect_equal(dsub, dstr)
  expect_equal(dstr, dlan)

  invisible(NULL)
}

test_letl()

