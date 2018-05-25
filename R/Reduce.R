
#' Use function to reduce or expand arguments.
#'
#' Use function \code{f} to reduce arguments \code{ars} (inspired by APL reduce/expand notation).
#' This is essentially passing multiple values from a list as multiple independent arguments to a
#' function.  So this can be used to supply a standard evaluation interface for variadic functions.
#' \code{f \%|.\% args} is roughly equivalent to \code{do.call(f, args)}.
#'
#' Note: these two operators are still experimental extensions.  They will
#' require more training material and use cases before they are fully
#' advised.
#'
#' @param f function
#' @param args argument list or vector
#' @return f(args) where args elements become individual arguments of f.
#'
#' @examples
#'
#' # basic examples
#' c(1, 2, 3) %.|% sum
#' c(1, 2, 3) %.|% base::sum
#' c(1, 2, 3) %.|% function(...) { sum(...) }
#'
#' # partial application of log(5, base=2)
#' 5 %.>% (c(., base=2) %.|% log)
#'
#' # # partial application with dplyr
#' # # can be used with dplyr/rlang as follows
#' # d <- data.frame(x=1, y=2, z=3)
#' # syms <- rlang::syms(c("x", "y"))
#' # d %.>% (c(list(.), syms) %.|% dplyr::select)
#'
#' @name reduceargs
NULL

reduceimpl <- function(f, args, env) {
  fnam <- deparse(f)
  if(is.name(f) || is.character(f)) {
    # name of function case as in c(1, 2, 3) %.|% sum
    f <- get(as.character(f), env)
  } else if(is.call(f) &&
            (length(f)==3) &&
            isTRUE(as.character(f[[1]])=="::") &&
            is.name(f[[2]]) && is.name(f[[3]])) {
    # package qualified name of function as in c(1, 2, 3) %.|% base::sum
    f <- eval(f, envir = env, enclos = env)
  } else if(is.call(f) &&
            isTRUE(as.character(f[[1]])=="function")) {
    # function def as in c(1, 2, 3) %.|% function(...) { sum(...) }
    f <- eval(f, envir = env, enclos = env)
  }
  if(!is.function(f)) {
    stop(paste("wrapr::reduceimpl f (", fnam, ") must de-refence to a function"))
  }
  if(!is.list(args)) {
    args <- as.list(args)
  }
  do.call(f, args, envir = env)
}

#' @describeIn reduceargs f reduce args
#' @export
`%|.%` <- function(f, args) {
  f <- substitute(f)
  env <- parent.frame()
  reduceimpl(f, args, env)
}

#' @describeIn reduceargs args expand f
#' @export
`%.|%` <- function(args, f) {
  f <- substitute(f)
  env <- parent.frame()
  reduceimpl(f, args, env)
}
