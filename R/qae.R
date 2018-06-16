
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
  e_terms <- substitute(list(...))
  if(length(setdiff(names(e_terms), ""))>0) {
    stop("wrapr::qe() unexpected names/arguments")
  }
  # e_terms is a list of k+1 items, first is "list" the rest are captured expressions
  len <- length(e_terms) # first slot is "list"
  if(len<=1) {
    return(c())
  }
  rhs <- vector(len-1, mode='list')
  for(i in (2:len)) {
    ei <- e_terms[[i]]
    rhs[[i-1]] <- wrapr_deparse(ei)
  }
  rhs
}


#' Quote assignment expressions (name = expr, name := expr, name \%:=\% expr).
#'
#' Accepts arbitrary un-parsed expressions as
#' assignments to allow forms such as "Sepal_Long := Sepal.Length >= 2 * Sepal.Width".
#' (without the quotes).
#' Terms are expressions of the form "lhs := rhs", "lhs = rhs", "lhs \%:=\% rhs".
#'
#' @param ... assignment expressions.
#' @return array of quoted assignment expressions.
#'
#' @seealso \code{\link{qc}}, \code{\link{qe}}
#'
#' @examples
#'
#' exprs <- qae(Sepal_Long := Sepal.Length >= ratio * Sepal.Width,
#'              Petal_Short = Petal.Length <= 3.5)
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
  ae_terms <- substitute(list(...))
  # ae_terms is a list of k+1 items, first is "list" the rest are captured expressions
  len <- length(ae_terms) # first slot is "list"
  if(len<=1) {
    return(c())
  }
  nms <- names(ae_terms)
  lhs <- vector(len-1, mode='list')
  rhs <- vector(len-1, mode='list')
  for(i in (2:len)) {
    ei <- ae_terms[[i]]
    ni <- nms[[i]]
    li <- length(ei)
    vi <- ""
    if((!is.null(ni)) && (!is.na(ni)) &&
       (is.character(ni)) && (nchar(ni)>0)) {
      vi <- wrapr_deparse(ei)
    } else {
      if((!(as.character(ei[[1]]) %in% c(':=', '%:=%'))) || (li<2)) {
        stop("wrapr::qae() terms must be of the form: sym := exprm, sym = expr, or sym %:=% expr")
      }
      ni <- as.character(ei[[2]])[[1]]
      if(li>2) {
        vi <- lapply(3:li,
                     function(j) {
                       wrapr_deparse(ei[[j]])
                     })
        vi <- paste(vi, collapse = "\n")
      }
    }
    if(is.null(ni)) {
      stop("seplyr::quote_mutate terms must all have names (either from =, :=, or %:=%)")
    }
    lhs[[i-1]] <- ni
    rhs[[i-1]] <- vi
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
  # as.character(paste(deparse(substitute(s)), collapse = '\n'))
  wrapr_deparse(substitute(s))
}
