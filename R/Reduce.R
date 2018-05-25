
#' Use function to reduce or expand arguments.
#'
#' \code{applyf} is a wrapper for \code{\link[base]{do.call}} that also accepts argument vectors.
#' The operator versions \code{\%.|\%} and \code{\%|.\%} are mere syntactic sugar.
#' In all cases any these functions are sufficient to pass arguments from a list to a variadic
#' function (such as \code{\link[base]{sum}}). The operator symbols are meant to invoke non-tilted
#' versions of APL's reduce and expand operators.
#'
#' @param f function.
#' @param args argument list or vector, entries expanded as function arguments.
#' @param env environment to execute in.
#' @return f(args) where args elements become individual arguments of f.
#'
#' @seealso \code{\link[base]{do.call}}
#'
#' @examples
#'
#' # basic examples
#' c(1, 2, 3) %.|% sum
#' c(1, 2, 3) %.|% base::sum
#' c(1, 2, 3) %.|% function(...) { sum(...) }
#'
#' # simulate partial application of log(5, base=2)
#' 5 %.>% applyf(log, list(., base = 2))
#'
#' # # simluate partial application with dplyr
#' # # can be used with dplyr/rlang as follows
#' # d <- data.frame(x=1, y=2, z=3)
#' # syms <- rlang::syms(c("x", "y"))
#' # d %.>% applyf(dplyr::select, c(list(.), syms))
#'
#' @export
#'
applyf <- function(f, args,
                   env = parent.frame()) {
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
    stop(paste("wrapr::applyf function argument f (", fnam, ") must de-refence to a function"))
  }
  if(!is.list(args)) {
    args <- as.list(args)
  }
  do.call(f, args, envir = env)
}

#' @describeIn applyf f reduce args
#' @export
`%|.%` <- function(f, args) {
  f <- substitute(f)
  env <- parent.frame()
  applyf(f, args, env)
}

#' @describeIn applyf args expand f
#' @export
`%.|%` <- function(args, f) {
  f <- substitute(f)
  env <- parent.frame()
  applyf(f, args, env)
}
