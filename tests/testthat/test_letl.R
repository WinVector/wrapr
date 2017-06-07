library('wrapr')

context("letl")

test_that("testl_let.R", {

  d <- data.frame(
    year= c(2005, 2005),
    name= c('a', 'a'),
    stringsAsFactors = FALSE)

  # dsub <- let(list(NEWCOL='val'),
  #             dplyr::mutate(d, NEWCOL = 7),
  #             subsMethod= 'subsubs'
  # )
  dstr <- let(list(NEWCOL='val'),
            dplyr::mutate(d, NEWCOL = 7),
            subsMethod= 'stringsubs'
  )
  dlan <- let(list(NEWCOL='val'),
            dplyr::mutate(d, NEWCOL = 7),
            subsMethod= 'langsubs'
  )
  #expect_equal(dsub, dstr)
  expect_equal(dstr, dlan)
})
