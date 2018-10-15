
#' Adapt a function to use bquote on its arguments.
#'
#' @param fn function to adapt, must have non-empty formals().
#' @return new function.
#'
#' @examples
#'
#' f1 <- function(x) { substitute(x) }
#' f2 <- bquote_function(f1)
#' arg <- as.name("USER_ARG")
#' f2(arg)    # returns arg
#' f2(.(arg)) # returns USER_ARG
#'
#' @export
#'
bquote_function <- function(fn) {
  frmls <- formals(fn)
  if(length(formals)<=0) {
    stop("wrapr::bquote_function function must have formals() not empty")
  }
  .wrapr_wrapped_function_ <- NULL # don't look unbound
  f <- function() {
    call <- match.call()
    env <- parent.frame()
    mc <- do.call(bquote, list(call, where = env), envir = env)
    mc[[1]] <- .wrapr_wrapped_function_
    eval(mc, envir = env)
  }
  formals(f) <- frmls
  newenv <- new.env(parent = environment(fn))
  assign('.wrapr_wrapped_function_', fn, envir = newenv)
  environment(f) <- newenv
  f
}
