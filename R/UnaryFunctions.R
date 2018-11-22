
# treat objects as partially applied functions using S4 and wrapr::`%.>%` (dot arrow pipe)

#' @importFrom methods new setClass setMethod signature
NULL



#' Functions that take a single argument
#' @export
setClass("UnaryFn")

#' Apply a single argument function to its argument.
#'
#' If x is a UnaryFn instance this function returns a new
#' UnaryFnList representing the composite function c(f, x)
#' which is interpreted as the function x(f(.)) (composition
#' from left to right).  Otherwise evaluate f(x) (application
#' from left to right).
#'
#' @param f object of S4 class UnaryFn
#' @param x argument.
#' @param env environment to work in.
#' @return f(x) if x is not a UnaryFn else f composed with x.
#'
#' @export
setGeneric(
  "ApplyTo",
  function(f, x, env = parent.frame()) {
    stop("default ApplyTo called")
  })


#' Apply right wrapped function to argument on left.
#'
#' @param pipe_left_arg left argument
#' @param pipe_right_arg substitute(pipe_right_arg) argument
#' @param pipe_environment environment to evaluate in
#' @param left_arg_name name, if not NULL name of left argument.
#' @param pipe_string character, name of pipe operator.
#' @param right_arg_name name, if not NULL name of right argument.
#' @return result
#'
#' @keywords internal
#'
#' @export
apply_right.UnaryFn <- function(pipe_left_arg,
                                pipe_right_arg,
                                pipe_environment,
                                left_arg_name,
                                pipe_string,
                                right_arg_name) {
  force(pipe_environment)
  ApplyTo(pipe_right_arg, pipe_left_arg, pipe_environment)
}


#' Apply right wrapped function to argument on left.
#'
#' @param pipe_left_arg left argument
#' @param pipe_right_arg substitute(pipe_right_arg) argument
#' @param pipe_environment environment to evaluate in
#' @param left_arg_name name, if not NULL name of left argument.
#' @param pipe_string character, name of pipe operator.
#' @param right_arg_name name, if not NULL name of right argument.
#' @return result
#'
#' @keywords internal
#'
#' @export
apply_left.UnaryFn <- function(pipe_left_arg,
                                pipe_right_arg,
                                pipe_environment,
                                left_arg_name,
                                pipe_string,
                                right_arg_name) {
  force(pipe_environment)
  pipe_right_arg <- eval(pipe_right_arg,
                         envir = pipe_environment,
                         enclos = pipe_environment)
  ApplyTo(pipe_right_arg, pipe_left_arg, pipe_environment)
}


#' List of Unary functions taken in order.
#' @export
setClass("UnaryFnList",
         contains = "UnaryFn",
         slots = c(items = "list"))

#' @rdname ApplyTo
#' @export
setMethod(
  "ApplyTo",
  signature(f = "UnaryFnList", x = "UnaryFnList"),
  function(f, x, env = parent.frame()) {
    new("UnaryFnList",
        items =  c(f@items, x@items))
  })

#' @rdname ApplyTo
#' @export
setMethod(
  "ApplyTo",
  signature(f = "UnaryFnList", x = "UnaryFn"),
  function(f, x, env = parent.frame()) {
    new("UnaryFnList",
        items =  c(f@items, list(x)))
  })

#' @rdname ApplyTo
#' @export
setMethod(
  "ApplyTo",
  signature(f = "UnaryFn", x = "UnaryFnList"),
  function(f, x, env = parent.frame()) {
    new("UnaryFnList",
        items =  c(list(f), x@items))
  })

#' @rdname ApplyTo
#' @export
setMethod(
  "ApplyTo",
  signature(f = "UnaryFn", x = "UnaryFn"),
  function(f, x, env = parent.frame()) {
    new("UnaryFnList",
        items =  list(f, x))
  })

#' @rdname ApplyTo
#' @export
setMethod(
  "ApplyTo",
  signature(f = "UnaryFnList", x = "ANY"),
  function(f, x, env = parent.frame()) {
    force(env)
    for(itm in f@items) {
      x <- ApplyTo(itm, x)
    }
    x
  })



#' Package qualified name of a function as a function.
#' @export
setClass("PartialNamedFn",
         contains = "UnaryFn",
         slots = c(fn_name = "character",
                   fn_package = "character",
                   arg_name = "character",
                   args = "list"))

#' @rdname ApplyTo
#' @export
setMethod(
  "ApplyTo",
  signature(f = "PartialNamedFn", x = "ANY"),
  function(f, x, env = parent.frame()) {
    force(env)
    fn = getExportedValue(f@fn_package, f@fn_name)
    if(is.null(fn)) {
      stop(paste0("ApplyTo(PartialNamedFn, ANY) could not find ",
                  f@fn_package, "::", f@fn_name))
    }
    argl <- list(x)
    names(argl) <- f@arg_name
    do.call(what = fn, args = c(as.list(f@args), argl), envir = env)
  })

#' @rdname ApplyTo
#' @export
setMethod(
  "ApplyTo",
  signature(f = "PartialNamedFn", x = "UnaryFnList"),
  function(f, x, env = parent.frame()) {
    new("UnaryFnList",
        items =  c(list(f), x@items))
  })

#' @rdname ApplyTo
#' @export
setMethod(
  "ApplyTo",
  signature(f = "PartialNamedFn", x = "UnaryFn"),
  function(f, x, env = parent.frame()) {
    new("UnaryFnList",
        items =  list(f, x))
  })



#' Function with partial arguments as a new single argument function.
#' @export
setClass(
  "PartialFunction",
  contains = "UnaryFn",
  slots = c(fn = "function",
            arg_name = "character",
            args = "list"))

#' @rdname ApplyTo
#' @export
setMethod(
  "ApplyTo",
  signature(f = "PartialFunction", x = "ANY"),
  function(f, x, env = parent.frame()) {
    force(env)
    fn = f@fn
    argl <- list(x)
    names(argl) <- f@arg_name
    do.call(what = fn, args = c(as.list(f@args), argl), envir = env)
  })

#' @rdname ApplyTo
#' @export
setMethod(
  "ApplyTo",
  signature(f = "PartialFunction", x = "UnaryFnList"),
  function(f, x, env = parent.frame()) {
    new("UnaryFnList",
        items =  c(list(f), x@items))
  })

#' @rdname ApplyTo
#' @export
setMethod(
  "ApplyTo",
  signature(f = "PartialFunction", x = "UnaryFn"),
  function(f, x, env = parent.frame()) {
    new("UnaryFnList",
        items =  list(f, x))
  })





# f <- new("PartialNamedFn",
#          fn_name = "exp",
#          fn_package = "base",
#          arg_name = "x",
#          args = list())
#
#
# f2 <- new("PartialFunction",
#           fn = base::sin,
#           arg_name = "x",
#           args = list())
#
# ApplyTo(f, 5)
#
# ApplyTo(f2, 5)
#
# f3 <- new("UnaryFnList", items = list(f, f2))
#
# ApplyTo(f3, 5)

