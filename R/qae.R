
#' Quote expressions.
#'
#' Accepts arbitrary un-parsed expressions as
#' to allow forms such as "Sepal.Length >= 2 * Sepal.Width".
#' (without the quotes).
#'
#' @param ... assignment expressions.
#' @return array of quoted assignment expressions.
#'
#' @seealso \code{\link{qc}}, \code{\link{qae}}
#'
#' @examples
#'
#' exprs <- qe(Sepal.Length >= ratio * Sepal.Width,
#'              Petal.Length <= 3.5)
#' print(exprs)
#'
#' @export
#'
qe <- function(...) {
  mutateTerms <- substitute(list(...))
  if(length(setdiff(names(mutateTerms), ""))>0) {
    stop("wrapr::qe() unexpected names/arguments")
  }
  # mutateTerms is a list of k+1 items, first is "list" the rest are captured expressions
  len <- length(mutateTerms) # first slot is "list"
  if(len<=1) {
    return(c())
  }
  rhs <- vector(len-1, mode='list')
  for(i in (2:len)) {
    ei <- mutateTerms[[i]]
    rhs[[i-1]] <- paste(as.character(deparse(ei)), collapse = "\n")
  }
  rhs
}


#' Quote assignment expressions.
#'
#' Accepts arbitrary un-parsed expressions as
#' assignments to allow forms such as "Sepal_Long := Sepal.Length >= 2 * Sepal.Width".
#' (without the quotes).
#' Terms are expressions of the form "lhs := rhs".
#'
#' @param ... assignment expressions.
#' @return array of quoted assignment expressions.
#'
#' @seealso \code{\link{qc}}, \code{\link{qe}}
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
    stop("wrapr::qae() all assignments must be of the form a := b, not a = b")
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
      stop("wrapr::qae() terms must be of the form: sym := expr")
    }
    lhs[[i-1]] <- as.character(ei[[2]])
    rhs[[i-1]] <- paste(as.character(deparse(ei[[3]])), collapse = "\n")
  }
  lhs := rhs
}


#' Quote a string.
#'
#' @param s expression to be quoted as a string.
#' @return character
#'
#' @examples
#'
#' qs(a == "x")
#'
#' @export
#'
qs <- function(s) {
  as.character(paste(deparse(substitute(s)), collapse = '\n'))
}
