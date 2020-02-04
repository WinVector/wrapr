

build_minus_fn_env <- function(env) {
  force(env)
  orig_minus <- get('-', mode = 'function', envir = env)
  minus_fn_to_name <- function(e1, e2) {
    if(!missing(e2)) {
      return(orig_minus(e1, e2))
    }
    if(is.name(e1)) {
      return(e1)
    }
    if(is.character(e1) && (length(e1)==1)) {
      return(as.name(e1))
    }
    orig_minus(e1)
  }
  env2 <- new.env(parent = env)
  assign('-', minus_fn_to_name, envir = env2)
  return(env2)
}



#' Near \code{eval(bquote(expr))} shortcut.
#'
#' Evaluate \code{expr} with \code{bquote} \code{.()} substitution.
#' Including \code{.(-x)} promoting \code{x}'s value from character to a name,
#' which is called "quote negation" (hence the minus-sign).
#'
#' @param expr expression to evaluate.
#' @param where environment to work in.
#' @return evaluated substituted expression.
#'
#' @examples
#'
#' if(requireNamespace('graphics', quietly = TRUE)) {
#'    angle = 1:10
#'    variable <- as.name("angle")
#'    fn_name <- 'sin'
#'    evalb(  plot(x = .(variable), y = .(-fn_name)(.(variable))) )
#' }
#'
#' @export
#'
evalb <- function(expr, where = parent.frame()) {
  force(where)
  env2 <- build_minus_fn_env(where)
  expr <- substitute(expr)  # can't set env, as that changes substitute's behavior
  exprq <- do.call(bquote, list(expr, where = env2))
  eval(exprq,
       envir = env2,
       enclos = env2)
}




#' Treat call argument as bquoted-values.
#'
#' Re-write call to evaluate \code{expr} with \code{bquote} \code{.()} substitution.
#' Uses convetion that := is considered a alias for =.
#' Including \code{.(-x)} promoting \code{x}'s value from character to a name,
#' which is called "quote negation" (hence the minus-sign).
#'
#' @param call result of match.call()
#' @param env environment to perform lookups in.
#' @return altered call
#'
#' @seealso \code{\link{bquote_function}}, \code{\link{bquote_call_args}}
#'
#' @keywords internal
#'
#' @export
#'
#'
bquote_call <- function(call, env = parent.frame()) {
  force(env)
  # perform bquote transform
  env2 <- build_minus_fn_env(env)
  mc <- do.call(bquote, list(call, where = env2), envir = env2)
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
#' Re-writes call args to evaluate \code{expr} with \code{bquote} \code{.()} substitution.
#' Including \code{.(-x)} promoting \code{x}'s value from character to a name,
#' which is called "quote negation" (hence the minus-sign).
#'
#' @param call result of match.call()
#' @param env environment to perform lookups in.
#' @return name list of values
#'
#' @seealso \code{\link{bquote_function}}
#'
#'
#' @examples
#'
#' f <- function(q, ...) {
#'   env = parent.frame()
#'   # match.call() best called in function context.
#'   captured_call <- match.call()
#'   captured_args <- bquote_call_args(captured_call, env)
#'   captured_args
#' }
#'
#' z <- "x"
#' y <- 5
#' qv <- 3
#'
#' # equivalent to f(3, x = 5)
#' f(.(qv), .(z) := .(y))
#'
#' # equivalent to f(q = 7)
#' qname <- 'q'
#' f(.(qname) := 7)
#'
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
#' bquote_function is for adapting a function defined elsewhere for bquote-enabled argument substitution.
#' Re-write call to evaluate \code{expr} with \code{bquote} \code{.()} substitution.
#' Uses convetion that := is considered a alias for =.
#' Including \code{.(-x)} promoting \code{x}'s value from character to a name,
#' which is called "quote negation" (hence the minus-sign).
#'
#' @param fn function to adapt, must have non-empty formals().
#' @return new function.
#'
#' @seealso \code{\link{bquote_call_args}}
#'
#' @examples
#'
#'
#' if(requireNamespace('graphics', quietly = TRUE)) {
#'   angle = 1:10
#'   variable <- as.name("angle")
#'   plotb <- bquote_function(graphics::plot)
#'   plotb(x = .(variable), y = sin(.(variable)))
#' }
#'
#'
#'
#' f1 <- function(x) { substitute(x) }
#' f2 <- bquote_function(f1)
#' arg <- "USER_ARG"
#' f2(arg)    # returns arg
#' f2(.(arg)) # returns "USER_ARG" (character)
#' f2(.(-arg)) # returns USER_ARG (name)
#'
#'
#' @export
#'
bquote_function <- function(fn) {
  if(!is.function(fn)) {
    stop("wrapr::bquote_function fn wasn't a function")
  }
  if(is.primitive(fn)) {
    stop("wrapr::bquote_function can not wrap fn as is.primitive(fn) is TRUE")
  }
  frmls <- formals(fn)
  if(length(frmls)<=0) {
    return(fn) # take no args, nothing to do
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

