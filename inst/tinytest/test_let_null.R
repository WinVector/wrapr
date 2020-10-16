
test_let_null <- function() {
  e1 = as.character(let(c("z"= "z"), dDT[,"x":=NULL], eval=FALSE))
  e2 = as.character(let(list(), dDT[,"x":=NULL], eval=FALSE))
  expect_equal(e1, e2)

  invisible(NULL)
}

test_let_null()
