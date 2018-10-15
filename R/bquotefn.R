
#' Adapt a function to use bquote on its arguments.
#'
#' @param fn function to adapt, must have non-empty formals().
#' @return new function.
#'
#' @examples
#'
#' f1 <- function(x) { substitute(x) }
#' f2 <- bquote_function(f1)
#' arg <- "USER_ARG"
#' f2(arg)    # returns arg
#' f2(.(arg)) # returns USER_ARG
#'
#' @export
#'
bquote_function <- function(fn) {
  frmls <- formals(fn)
  if(length(formals)<=0) {
    stop("bquote_function function must have formals() not empty")
  }
  f <- function() {
    call <- match.call()
    env <- parent.frame()
    mc <- do.call(bquote, list(call, where = env), envir = env)
    mc[[1]] <- fn
    eval(mc, envir = env)
  }
  formals(f) <- frmls
  f
}
