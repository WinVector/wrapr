
#' Use function to reduce or expand arguments.
#'
#' The operators \code{\%.|\%} and \code{\%|.\%} are wrappers for \code{\link[base]{do.call}}.
#' These functions are used to pass arguments from a list to variadic
#' function (such as \code{\link[base]{sum}}). The operator symbols are meant to invoke non-tilted
#' versions of APL's reduce and expand operators. Unevaluated expressions containing
#' \code{\%.|\%}, \code{\%|.\%}, or \code{\link[base]{do.call}} can be used simulate partial function
#' application or simulate function Currying.  The take-away is one can delegate all
#' variadic argument construction to \code{\link[base]{list}}, and manipulation to \code{\link[base]{c}}.
#'
#' @param f function.
#' @param args argument list or vector, entries expanded as function arguments.
#' @return f(args) where args elements become individual arguments of f.
#'
#' @seealso \code{\link[base]{do.call}}, \code{\link[base]{list}}, \code{\link[base]{c}}
#'
#' @examples
#'
#' # basic examples
#' 1:10 %.|% sum
#' 1:10 %.|% base::sum
#' 1:10 %.|% function(...) { sum(...) }
#'
#' # simulate partial application of log(., base=2)
#' 1:4 %.>% do.call(log, list(., base = 2))
#'
#' # # simulate partial application with dplyr
#' # # can be used with dplyr/rlang as follows
#' # d <- data.frame(x=1, y=2, z=3)
#' # syms <- rlang::syms(c("x", "y"))
#' # d %.>% do.call(dplyr::select, c(list(.), syms))
#'
#' @name reduceexpand
NULL


reduceexpand <- function(f, args,
                   env = parent.frame()) {
  fnam <- wrapr_deparse(f)
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
    stop(paste("wrapr::reduceexpand function argument f (", fnam, ") must de-reference to a function"))
  }
  if(!is.list(args)) {
    args <- as.list(args)
  }
  do.call(f, args, envir = env)
}

#' @describeIn reduceexpand f reduce args
#' @export
`%|.%` <- function(f, args) {
  f <- substitute(f)
  env <- parent.frame()
  reduceexpand(f, args, env)
}

#' @describeIn reduceexpand args expand f
#' @export
`%.|%` <- function(args, f) {
  f <- substitute(f)
  env <- parent.frame()
  reduceexpand(f, args, env)
}
