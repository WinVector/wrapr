
#' Treat ... argument as bquoted-values.
#'
#' bquote_call_args is a helper to allow the user to write functions with bquote-enabled argument substitution.
#' Uses convetion that := is considered a alias for =.
#'
#' Note: eagerly evalutes argument and writes them into the function's
#' executing environment.
#'
#' @param call result of match.call()
#' @return name list of values
#'
#' @section \code{\link{bquote_function}}
#'
#' @examples
#'
#' f <- function(q, ...) {
#'   # match.call() best called in function context.
#'   captured_call <- match.call()
#'   captured_args <- bquote_call_args(captured_call)
#'   for(nmi in setdiff(ls(),
#'                      c("captured_call", "captured_args"))) {
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
bquote_call_args <- function(call) {
  # perform bquote transform
  env <- parent.frame()
  mc <- do.call(bquote, list(call, where = env), envir = env)
  args <- as.list(mc[-1])
  # map a := b to name(a) = b
  fixpos <- which(vapply(args,
                         function(ai) {
                           is.call(ai) && (as.character(ai)[[1]]==":=")
                         }, logical(1)))
  if(length(fixpos)>0) {
    if(length(intersect(names(args)[fixpos], names(args)[!fixpos]))>0) {
      stop("wrapr::bquote_dots := and = names must be disjoint") # TODO: see if we need this
    }
    nms <- vapply(args[fixpos],
                  function(ai) {
                    as.character(ai[[2]])
                  }, character(1))
    vals <- lapply(args[fixpos],
                   function(ai) {
                     ai[[3]]
                   })
    names(args)[fixpos] <- nms
    args[fixpos] <- vals
  }
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
#' @section \code{\link{bquote_call_args}}
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

