
test_let_null <- function() {
  e1 = as.character(let(c("z"= "z"), dDT[,"x":=NULL], eval=FALSE))
  e2 = as.character(let(list(), dDT[,"x":=NULL], eval=FALSE))
  RUnit::checkEquals(e1, e2)

  invisible(NULL)
}