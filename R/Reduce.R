
#' Use function to reduce or expand arguments.
#'
#' \code{x \%.|\% f} stands for \code{f(x[[1]], x[[2]], ..., x[[length(x)]])}.
#' \code{v \%|.\% x} also stands for \code{f(x[[1]], x[[2]], ..., x[[length(x)]])}.
#' The two operators are the same, the variation just allowing the user to choose the order they write things.
#' The mnemonic is: "data goes on the dot-side of the operator."
#'
#' Note: the reduce operation is implemented by \code{do.call()}, so has
#' standard R named argument semantics.
#'
#' @param f function.
#' @param args argument list or vector, entries expanded as function arguments.
#' @return f(args) where args elements become individual arguments of f.
#'
#' @seealso \code{\link[base]{do.call}}, \code{\link[base]{list}}, \code{\link[base]{c}}
#'
#' @examples
#'
#' args <- list('prefix_', c(1:3), '_suffix')
#' args %.|% paste0
#' # prefix_1_suffix" "prefix_2_suffix" "prefix_3_suffix"
#' paste0 %|.% args
#' # prefix_1_suffix" "prefix_2_suffix" "prefix_3_suffix"
#'
#' @name reduceexpand
NULL


reduceexpand <- function(f, args,
                   env = parent.frame()) {
  force(env)
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
