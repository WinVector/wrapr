
#' Treat ... call argument as bquoted-values.
#'
#' bquote_call re-writes calls.
#'
#' Note: eagerly evalutes argument and writes them into the function's
#' executing environment.
#'
#' @param call result of match.call()
#' @param env environment to perform lookups in.
#' @return altered call
#'
#' @seealso \code{\link{bquote_function}}, \code{\link{bquote_call_args}}
#'
#' @keywords internal
#'
#'
#' @export
#'
#'
bquote_call <- function(call, env = parent.frame()) {
  force(env)
  # perform bquote transform
  mc <- do.call(bquote, list(call, where = env), envir = env)
  # map a := b to name(a) = b
  fixpos <- which(vapply(mc[-1],
                         function(ai) {
                           is.call(ai) && (as.character(ai)[[1]]==":=")
                         }, logical(1)))
  if(length(fixpos)>0) {
    fixpos <- fixpos + 1
    if(length(intersect(names(mc)[fixpos], names(mc)[!fixpos]))>0) {
      stop("wrapr::bquote_call := and = names must be disjoint")
    }
    nms <- vapply(mc[fixpos],
                  function(ai) {
                    as.character(ai[[2]])
                  }, character(1))
    vals <- lapply(mc[fixpos],
                   function(ai) {
                     ai[[3]]
                   })
    names(mc)[fixpos] <- nms
    mc[fixpos] <- vals
  }
  mc
}


#' Treat ... argument as bquoted-values.
#'
#' bquote_call_args is a helper to allow the user to write functions with bquote-enabled argument substitution.
#' Uses convetion that := is considered a alias for =.
#'
#' Note: eagerly evalutes argument and writes them into the function's
#' executing environment.
#'
#' @param call result of match.call()
#' @param env environment to perform lookups in.
#' @return name list of values
#'
#' @seealso \code{\link{bquote_function}}
#'
#' @examples
#'
#' f <- function(q, ...) {
#'   env = parent.frame()
#'   # match.call() best called in function context.
#'   captured_call <- match.call()
#'   captured_args <- bquote_call_args(captured_call, env)
#'   for(nmi in setdiff(ls(),
#'                      c("captured_call", "captured_args", "env"))) {
#'     print(paste(nmi, get(nmi)))
#'   }
#'   captured_args
#' }
#'
#' z <- "x"
#' y <- 5
#' qv <- 3
#'
#' # equivilent to f(3, x = 5)
#' args <- f(q = .(qv), .(z) := .(y))
#'
#' print(args)
#'
#' @export
#'
#'
bquote_call_args <- function(call, env = parent.frame()) {
  force(env)
  args <- as.list(wrapr::bquote_call(call, env)[-1])
  resframe <- parent.frame()
  for(i in seq_len(length(args))) {
    nmi <- names(args)[[i]]
    if((!is.null(nmi)) && (!is.na(nmi)) && (nchar(nmi)>0)) {
      vali <- args[[i]]
      assign(nmi, vali, envir = resframe)
    }
  }
  args
}




#' Adapt a function to use bquote on its arguments.
#'
#' bquote_function is for adapting a function defined elsewhere for bquite-enabled argument substitution.
#'
#' @param fn function to adapt, must have non-empty formals().
#' @return new function.
#'
#' @seealso \code{\link{bquote_call_args}}
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
    mc <- wrapr::bquote_call(call, env)
    mc[[1]] <- .wrapr_wrapped_function_
    eval(mc, envir = env)
  }
  formals(f) <- frmls
  newenv <- new.env(parent = environment(fn))
  assign('.wrapr_wrapped_function_', fn, envir = newenv)
  environment(f) <- newenv
  f
}

#' eval(bquote(expr)) shortcut.
#'
#' @param ... expression to evaluate (one argument).
#' @param where environment to work in.
#' @return eval(bquote(expr))
#'
#' @examples
#'
#' x = 5
#' y = 2
#' evalb(.(x) + .(y))
#'
#' @export
#'
evalb <- function(..., where = parent.frame()) {
  force(where)
  exprq <- bquote(..., where = where)
  eval(exprq,
       envir = where,
       enclos = where)
}

