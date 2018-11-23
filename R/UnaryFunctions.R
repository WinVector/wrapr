
# treat objects as partially applied functions using S4 and wrapr::`%.>%` (dot arrow pipe)

#' @importFrom methods new setClass setMethod signature show is
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
    stop(paste("default ApplyTo(f, x, env) called with classes",
               "f:{", paste(class(f), collapse = ", "), "}",
               "x:{", paste(class(x), collapse = ", "), "}"))
  })


# build a list of all UnaryFn from possibly composite
concat_items_rev <- function(op1, op2) {
  if(!is.list(op1)) {
    op1 <- list(op1)
  }
  if(!is.list(op2)) {
    op2 <- list(op2)
  }
  c(op2, op1)
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








is_list_of_unaryfns <- function(object) {
  items <- object@items
  if(!is.list(items)) {
    return("items must be a list")
  }
  for(i in seq_len(length(items))) {
    item_i <- items[[i]]
    if((!isS4(item_i)) || (!is(item_i, "UnaryFn"))) {
      return(paste("item ", i, " must be an instance of a class derived from UnaryFn"))
    }
    s4class <- as.character(class(item_i))
    if(length(s4class)!=1) {
      return(paste("item ", i, " must have single class name"))
    }
    if(s4class %in% c("UnaryFn", "UnaryFnList")) {
      return(paste("item ", i, " must not be of class ", s4class))
    }
  }
  return(character(0))
}

#' List of Unary functions taken in order.
#' @export
setClass("UnaryFnList",
         contains = "UnaryFn",
         slots = c(items = "list"),
         validity = is_list_of_unaryfns)

#' @rdname ApplyTo
#' @export
setMethod(
  "ApplyTo",
  signature(f = "UnaryFnList", x = "UnaryFnList"),
  function(f, x, env = parent.frame()) {
    new("UnaryFnList",
        items =  concat_items_rev(f@items, x@items))
  })

#' @rdname ApplyTo
#' @export
setMethod(
  "ApplyTo",
  signature(f = "UnaryFnList", x = "UnaryFn"),
  function(f, x, env = parent.frame()) {
    new("UnaryFnList",
        items =  concat_items_rev(f@items, list(x)))
  })

#' @rdname ApplyTo
#' @export
setMethod(
  "ApplyTo",
  signature(f = "UnaryFn", x = "UnaryFnList"),
  function(f, x, env = parent.frame()) {
    new("UnaryFnList",
        items =  concat_items_rev(list(f), x@items))
  })

#' @rdname ApplyTo
#' @export
setMethod(
  "ApplyTo",
  signature(f = "UnaryFn", x = "UnaryFn"),
  function(f, x, env = parent.frame()) {
    new("UnaryFnList",
        items =  concat_items_rev(list(f), list(x)))
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

#' format step
#'
#' @param x object to format
#' @param ... additional aguments (not used)
#' @return character
#'
#' @export
format.UnaryFnList <- function(x, ...) {
  fns <- vapply(x@items,
                format,
                character(1))
  paste0("UnaryFnList",
         "(\n   ",
         paste(fns, collapse = ",\n   "),
         ")")
}

#' S4 print method
#'
#' @param object item to print
#'
#' @export
setMethod(
  f = "show",
  signature = "UnaryFnList",
  definition = function(object) {
    print(format(object))
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
        items =  concat_items_rev(list(f), x@items))
  })

#' @rdname ApplyTo
#' @export
setMethod(
  "ApplyTo",
  signature(f = "PartialNamedFn", x = "UnaryFn"),
  function(f, x, env = parent.frame()) {
    new("UnaryFnList",
        items =  concat_items_rev(list(f), list(x)))
  })


#' format step
#'
#' @param x object to format
#' @param ... additional aguments (not used)
#' @return character
#'
#' @export
format.PartialNamedFn <- function(x, ...) {
  paste0( x@fn_package, "::", x@fn_name,
          "(",
          x@arg_name, "=., ",
          paste(names(x@args), collapse = ", "),
          ")")
}

#' S4 print method
#'
#' @param object item to print
#'
#' @export
setMethod(
  f = "show",
  signature = "PartialNamedFn",
  definition = function(object) {
    print(format(object))
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
        items =  concat_items_rev(list(f), x@items))
  })

#' @rdname ApplyTo
#' @export
setMethod(
  "ApplyTo",
  signature(f = "PartialFunction", x = "UnaryFn"),
  function(f, x, env = parent.frame()) {
    new("UnaryFnList",
        items =  concat_items_rev(list(f), list(x)))
  })


#' format step
#'
#' @param x object to format
#' @param ... additional aguments (not used)
#' @return character
#'
#' @export
format.PartialFunction <- function(x, ...) {
  paste0("PartialFunction",
         "(",
         x@arg_name, "=., ",
         paste(names(x@args), collapse = ", "),
         ")")
}

#' S4 print method
#'
#' @param object item to print
#'
#' @export
setMethod(
  f = "show",
  signature = "PartialFunction",
  definition = function(object) {
    print(format(object))
  })






#' Code text as a new partial function.
#' @export
setClass(
  "SrcFunction",
  contains = "UnaryFn",
  slots = c(expr_src = "character",
            arg_name = "character",
            args = "list"))

#' @rdname ApplyTo
#' @export
setMethod(
  "ApplyTo",
  signature(f = "SrcFunction", x = "ANY"),
  function(f, x, env = parent.frame()) {
    force(env)
    expr_src <- f@expr_src
    eval_env <- new.env(parent = env)
    for(ni in names(f@args)) {
      vi <- f@args[[ni]]
      assign(ni, vi, envir = eval_env)
    }
    assign(f@arg_name, x, envir = eval_env)
    eval(parse(text = expr_src),
         envir = eval_env,
         enclos = eval_env)
  })

#' @rdname ApplyTo
#' @export
setMethod(
  "ApplyTo",
  signature(f = "SrcFunction", x = "UnaryFnList"),
  function(f, x, env = parent.frame()) {
    new("UnaryFnList",
        items =  concat_items_rev(list(f), x@items))
  })

#' @rdname ApplyTo
#' @export
setMethod(
  "ApplyTo",
  signature(f = "SrcFunction", x = "UnaryFn"),
  function(f, x, env = parent.frame()) {
    new("UnaryFnList",
        items =  concat_items_rev(list(f), list(x)))
  })


#' format step
#'
#' @param x object to format
#' @param ... additional aguments (not used)
#' @return character
#'
#' @export
format.SrcFunction <- function(x, ...) {
  paste0("SrcFunction{ ",
         x@expr_src,
         " }(",
         x@arg_name, "=., ",
         paste(names(x@args), collapse = ", "),
         ")")
}

#' S4 print method
#'
#' @param object item to print
#'
#' @export
setMethod(
  f = "show",
  signature = "SrcFunction",
  definition = function(object) {
    print(format(object))
  })




