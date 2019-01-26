
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
#' @param f object of S4 class derived from UnaryFn.
#' @param x argument.
#' @param env environment to work in.
#' @return f(x) if x is not a UnaryFn else f composed with x.
#'
#' @export
#'
setGeneric(
  "ApplyTo",
  function(f, x, env = parent.frame()) {
    stop(paste("default ApplyTo(f, x, env) called with classes",
               "f:{", paste(class(f), collapse = ", "), "}",
               "x:{", paste(class(x), collapse = ", "), "}"))
  })


#' build a list of all UnaryFn from possibly composite
#'
#' @param op1 list1
#' @param op2 list2
#' @return c(list2, list1)
#'
#' @keywords internal
#'
#' @export
#'
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
#' @param pipe_left_arg left argument.
#' @param pipe_right_arg pipe_right_arg argument, class derived from UnaryFn.
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
  if((!isS4(pipe_right_arg)) || (!methods::is(pipe_right_arg, "UnaryFn"))) {
    stop(paste("wrapr::apply_right.UnaryFn right argument: ", pipe_right_arg, " must be an instance of a class derived from UnaryFn"))
  }
  if("relop" %in% class(pipe_left_arg)) {
    stop("attempt to pipe a relop into a wrapr::UnaryFn, please use rqdatatable::rq_fn_wrapper() to wrap the relop or rqdatatable::rq_ufn() to wrap the UnaryFn")
  }
  ApplyTo(pipe_right_arg, pipe_left_arg, pipe_environment)
}


#' Apply right wrapped function to argument on left.
#'
#' @param pipe_left_arg left argument should be a class derived from UnaryFn.
#' @param pipe_right_arg substitute(pipe_right_arg) argument, should evaluate to a class derived from UnaryFn.
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
  if("relop" %in% class(pipe_right_arg)) {
    stop("attempt to pipe a wrapr::UnaryFn into a relop, please use rqdatatable::rq_fn_wrapper() to wrap the relop or rqdatatable::rq_ufn() to wrap the UnaryFn")
  }
  if((!isS4(pipe_left_arg)) || (!methods::is(pipe_left_arg, "UnaryFn"))) {
    stop(paste("wrapr::apply_left.UnaryFn left argument: ", pipe_left_arg, " must be an instance of a class derived from UnaryFn"))
  }
  if((!isS4(pipe_right_arg)) || (!methods::is(pipe_right_arg, "UnaryFn"))) {
    stop(paste("wrapr::apply_left.UnaryFn right argument: ", pipe_right_arg, " must be an instance of a class derived from UnaryFn"))
  }
  ApplyTo(pipe_right_arg, pipe_left_arg, pipe_environment)
}








is_list_of_unaryfns <- function(object) {
  items <- object@items
  if(!is.list(items)) {
    return("items must be a list")
  }
  for(i in seq_len(length(items))) {
    item_i <- items[[i]]
    if((!isS4(item_i)) || (!methods::is(item_i, "UnaryFn"))) {
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

# probably redundant, but can check there are no UnaryFns in the list
# later if we want.
args_is_a_okay_list <- function(object) {
  args <- object@args
  if(!is.list(args)) {
    return("items must be a list")
  }
  for(arg in args) {
    if(isS4(arg) && methods::is(arg, "UnaryFn")) {
      return("args must not be UnaryFn derived objects")
    }
  }
  return(character(0))
}



#' List of Unary functions taken in order.
#'
#' Unary functions are evaluated in left to right or first to last order.
#'
#' @export
setClass(
  "UnaryFnList",
  contains = "UnaryFn",
  slots = c(items = "list"),
  validity = is_list_of_unaryfns)


#' Wrap a list of UnaryFns as a UnaryFnList.
#'
#' Unary functions are evaluated in left to right or first to last order.
#'
#' @param items list of UnaryFn derived instances.
#' @param env environment to work in.
#' @return UnaryFnList
#'
#' @seealso \code{\link{pkgfn}}, \code{\link{wrapfn}}, \code{\link{srcfn}}
#'
#' @examples
#'
#' f <- as_fnlist(list(pkgfn("base::sin", "x"), pkgfn("base::cos", "x")))
#' cat(format(f))
#' 1:3 %.>% f
#'
#' @export
#'
as_fnlist <- function(items, env = parent.frame()) {
  force(env)
  # get odd cases where user has passed us a single UnaryFn
  if(isS4(items) && methods::is(items, "UnaryFn")) {
    if(methods::is(items, "UnaryFnList")) {
      return(items)
    }
    return(new(
      "UnaryFnList",
      items = list(items)))
  }
  if(!is.list(items)) {
    stop("wrapr::as_fnlist items must be a UnaryFn derived class or list of such")
  }
  x <- new(
    "UnaryFnList",
     items = list()
  )
  for(itm in items) {
    x <- ApplyTo(itm, x, env = env)
  }
  x
}

#' Convert a list of UnaryFns into a UnaryFn.
#'
#' Unary functions are evaluated in left to right or first to last order.
#'
#' @param items list of UnaryFn derived instances.
#' @param env environment to work in.
#' @return UnaryFnList
#'
#' @seealso \code{\link{pkgfn}}, \code{\link{wrapfn}}, \code{\link{srcfn}}
#'
#' @examples
#'
#' f <- as.UnaryFn(list(pkgfn("base::sin", "x"), pkgfn("base::cos", "x")))
#' cat(format(f))
#' 1:3 %.>% f
#'
#' @export
#'
as.UnaryFn <- function(items, env = parent.frame()) {
  force(env)
  # get odd cases where user has passed us a single UnaryFn
  if(isS4(items) && methods::is(items, "UnaryFn")) {
    return(items)
  }
  if(!is.list(items)) {
    stop("wrapr::as.UnaryFn items must be a UnaryFn derived class or list of such")
  }
  x <- new(
    "UnaryFnList",
    items = list()
  )
  for(itm in items) {
    x <- ApplyTo(itm, x, env = env)
  }
  x
}

#' Wrap a list of functions as a function.
#'
#' Unary functions are evaluated in left to right or first to last order.
#'
#' @param ... UnaryFn derived instances.
#' @return UnaryFnList
#'
#' @seealso \code{\link{pkgfn}}, \code{\link{wrapfn}}, \code{\link{srcfn}}
#'
#' @examples
#'
#' f <- fnlist(pkgfn("base::sin", "x"), pkgfn("base::cos", "x"))
#' cat(format(f))
#' 1:3 %.>% f
#'
#' @export
#'
fnlist <- function(...) {
  items <- list(...)
  env = parent.frame()
  wrapr::as_fnlist(items = items, env = env)
}


#' Combine UnaryFns
#'
#' @param ... UnaryFn derived classes to combine
#' @return UnaryFn representing the sequence
#'
#' @examples
#'
#' c(pkgfn("base::sin", "x"), pkgfn("base::cos", "x"))
#'
#' @export
#'
c.UnaryFn <- function(...) {
  items <- list(...)
  if(length(items)<=1) {
    if(length(items)==1) {
      return(items[[1]])
    }
    return(items)
  }
  env <- parent.frame()
  wrapr::as_fnlist(items = items, env = env)
}

#' Get list of primative unary fns.
#'
#' @param x UnaryFn derived classe to extract
#' @param ... not used.
#' @return list of non UnaryFnList functions
#'
#' @examples
#'
#' as.list(pkgfn("base::sin", "x"))
#' as.list(c(pkgfn("base::sin", "x"), pkgfn("base::cos", "x")))
#'
#' @export
#'
as.list.UnaryFnList <- function(x, ...) {
  wrapr::stop_if_dot_args(substitute(list(...)), "wrapr::as.list.UnaryFnList")
  x@items
}

#' Get list of primative unary fns.
#'
#' @param x UnaryFn derived classe to extract
#' @param ... not used.
#' @return list of non UnaryFnList functions
#'
#' @examples
#'
#' as.list(pkgfn("base::sin", "x"))
#' as.list(c(pkgfn("base::sin", "x"), pkgfn("base::cos", "x")))
#'
#' @export
#'
as.list.UnaryFn <- function(x, ...) {
  wrapr::stop_if_dot_args(substitute(list(...)), "wrapr::as.list.UnaryFn")
  list(x)
}




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
      x <- ApplyTo(itm, x, env = env)
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
setClass(
  "PartialNamedFn",
  contains = "UnaryFn",
  slots = c(fn_name = "character",
            fn_package = "character",
            arg_name = "character",
            args = "list"),
  validity = args_is_a_okay_list)


#' Wrap the name of a function as a function.
#'
#' @param fname character, function name in fname or package::fname format.
#' @param arg_name characer, name of argument to assign.
#' @param args named list of adittional arguments and values.
#' @return PartialNamedFn
#'
#' @seealso \code{\link{fnlist}}, \code{\link{wrapfn}}, \code{\link{srcfn}}
#'
#' @examples
#'
#' f <- pkgfn("base::sin", "x")
#' cat(format(f))
#' 1:3 %.>% f
#'
#' @export
#'
pkgfn <- function(fname, arg_name = ".", args = list()) {
  parts <- strsplit(fname, '::', fixed = TRUE)[[1]]
  if(length(parts)==1) {
    parts <- c("base", parts)
  }
  if(length(parts)!=2) {
    stop("pkgfn fname not in correct format")
  }
  if(!is.list(args)) {
    stop("wrapr::pkgfn args should be a list")
  }
  new(
    "PartialNamedFn",
    fn_name = parts[[2]],
    fn_package = parts[[1]],
    arg_name = arg_name,
    args = args
  )
}

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
    do.call(what = fn, args = c(argl, f@args), envir = env)
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
  paste0(x@fn_package, "::", x@fn_name,
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
            fn_text = "character",
            arg_name = "character",
            args = "list"),
  validity = args_is_a_okay_list)

#' Wrap the source for an exprssion as a function.
#'
#' @param fn function.
#' @param arg_name characer, name of argument to assign.
#' @param args named list of adittional arguments and values.
#' @return PartialFunction
#'
#' @seealso \code{\link{pkgfn}}, \code{\link{fnlist}}, \code{\link{srcfn}}
#'
#' @examples
#'
#' f <- wrapfn(sin, "x")
#' cat(format(f))
#' 1:3 %.>% f
#'
#' @export
#'
wrapfn <- function(fn, arg_name = ".", args = list()) {
  fn_text <- paste(deparse(substitute(fn)), collapse = " ")
  if(!is.list(args)) {
    stop("wrapr::wrapfn args should be a list")
  }
  new(
    "PartialFunction",
    fn = fn,
    fn_text = fn_text,
    arg_name = arg_name,
    args = args
  )
}

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
    do.call(what = fn, args = c(argl, f@args), envir = env)
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
  paste0("PartialFunction{",
         paste(x@fn_text, collapse = "\n   "),
         "}(",
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
            args = "list"),
  validity = args_is_a_okay_list)

#' Wrap the source for an exprssion as a function.
#'
#' @param expr_src character, source code of expresson.
#' @param arg_name characer, name of argument to assign.
#' @param args named list of adittional arguments and values.
#' @return SrcFunction
#'
#' @seealso  \code{\link{fnlist}}, \code{\link{pkgfn}}, \code{\link{wrapfn}}
#'
#' @examples
#'
#' f <- srcfn(". + z", ".", args = list(z = 10))
#' cat(format(f))
#' 1:3 %.>% f
#'
#'
#' @export
#'
srcfn <- function(expr_src, arg_name = ".", args = list()) {
  new(
    "SrcFunction",
    expr_src = expr_src,
    arg_name = arg_name,
    args = args
  )
}



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
         paste(x@expr_src, collapse = "\n   "),
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




