
#' Quote assignment expressions.
#'
#' Accepts arbitrary un-parsed expressions as
#' assignments to allow forms such as "Sepal.Length >= 2 * Sepal.Width".
#' (without the quotes).
#' Terms are vectors or lists of the form "lhs := rhs".
#'
#'
#' @param ... assignment expressions.
#' @return array of quoted assignment expressions.
#'
#' @examples
#'
#' exprs <- qae(Sepal_Long := Sepal.Length >= ratio * Sepal.Width,
#'              Petal_Short := Petal.Length <= 3.5)
#' print(exprs)
#' #ratio <- 2
#' #datasets::iris %.>%
#' #  seplyr::mutate_se(., exprs) %.>%
#' #  summary(.)
#'
#' @export
#'
qae <- function(...) {
  # convert char vector into spliceable vector
  # from: https://github.com/tidyverse/rlang/issues/116
  mutateTerms <- substitute(list(...))
  if(length(setdiff(names(mutateTerms), ""))>0) {
    stop("seplyr::exprs() all assignments must be of the form a := b, not a = b")
  }
  # mutateTerms is a list of k+1 items, first is "list" the rest are captured expressions
  len <- length(mutateTerms) # first slot is "list"
  if(len<=1) {
    return(c())
  }
  lhs <- vector(len-1, mode='list')
  rhs <- vector(len-1, mode='list')
  for(i in (2:len)) {
    ei <- mutateTerms[[i]]
    if((length(ei)!=3)||(as.character(ei[[1]])!=':=')) {
      stop("seplyr::exprs() terms must be of the form: sym := expr")
    }
    lhs[[i-1]] <- as.character(ei[[2]])
    rhs[[i-1]] <- as.character(deparse(ei[[3]]))
  }
  lhs := rhs
}

