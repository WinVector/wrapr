library('wrapr')

context("letl")

test_that("testl_let.R", {

  d <- data.frame(
    year= c(2005, 2005),
    name= c('a', 'a'),
    stringsAsFactors = FALSE)

  ds <- let(list(NEWCOL='val'),
            dplyr::mutate(d, NEWCOL = 7),
            subsMethod= 'stringsubs'
  )
  dl <- let(list(NEWCOL='val'),
            dplyr::mutate(d, NEWCOL = 7),
            subsMethod= 'langsubs'
  )
  expect_equal(ds, dl)
})
