

# treat objects as partially applied functions

#' @importFrom methods new setClass setMethod signature
NULL

#' Wrap a function and arguments in an object for later S3 pipe dispatch.
#'
#' @param fn function to wrap
#' @param ... force later arguments to be taken by name.
#' @param arg_name name for remaining argument.
#' @param args list of function argument values
#' @return wrapped function
#'
#' @seealso \code{\link{applyto}}
#'
#' @export
#'
wrap_function_S3 <- function(fn,
                             ...,
                             arg_name = '', args = list()) {
  stop_if_dot_args(substitute(list(...)), "wrapr::wrap_function_S3")
  o <- list(fn = fn, arg_name = arg_name, args = args)
  class(o) <- "wrapr_funobj_S3"
  o
}

#' Wrap a function name and arguments in an object for later S3 pipe dispatch.
#'
#' @param fn_name character, name of function.
#' @param ... force later arguments to be taken by name.
#' @param fn_package character, name of package.
#' @param arg_name name for remaining argument.
#' @param args list of function argument values
#' @return wrapped function
#'
#' @seealso \code{\link{applyto}}
#'
#' @export
#'
wrap_fname_S3 <- function(fn_name = NULL,
                          ...,
                          fn_package = "base",
                          arg_name = '', args = list()) {
  stop_if_dot_args(substitute(list(...)), "wrapr::wrap_fname_S3")
  o <- list(fn_name = fn_name, fn_package = fn_package, args = args)
  class(o) <- "wrapr_funobj_S3"
  o
}


get_wrapr_funobj_S3_portion <- function(wfn) {
  if("wrapr_funobj_S3" %in% class(wfn)) {
    return(wfn)
  }
  if(!isS4(wfn)) {
    return(NULL)
  }
  tryCatch({
    wfn <- wfn@wfn
    },
    error = function(e) {e})
  if("wrapr_funobj_S3" %in% class(wfn)) {
    return(wfn)
  }
  return(NULL)
}

#' Apply a wrapped function to an argument.
#'
#' @param wfn wrapped function.
#' @param arg additional argument value.
#' @param env environment to evaluate in.
#' @return wfn applied to arg.
#'
#' @seealso \code{\link{wrap_function_S3}}, \code{\link{wrap_fname_S3}}, \code{\link{wrap_function_S4}}, \code{\link{wrap_fname_S4}}
#'
#' @export
#'
applyto <- function(wfn, arg, env = parent.frame()) {
  force(env)
  wfn <- get_wrapr_funobj_S3_portion(wfn)
  if(!("wrapr_funobj_S3" %in% class(wfn))) {
    stop("wrapr::applyto wfn must be of class wrapr_funobj_S3 or a class derived from def_funobj_s4_class()")
  }
  if(!is.null(wfn$fn)) {
    fn = wfn$fn
  } else {
    fn = getExportedValue(wfn$fn_package, wfn$fn_name)
  }
  argl <- list(arg)
  names(argl) <- wfn$arg_name
  do.call(what = fn, args = c(as.list(wfn$args), argl), envir = env)
}


#' Apply a list of wrapped functions to an argument.
#'
#' @param lwfn list of wrapped functions
#' @param arg additional argument value.
#' @param env environment to evaluate in.
#' @return wfn applied to arg.
#'
#' @seealso \code{\link{wrap_function_S3}}, \code{\link{wrap_fname_S3}}, \code{\link{wrap_function_S4}}, \code{\link{wrap_fname_S4}}
#'
#' @export
#'
lapplyto <- function(lwfn, arg, env = parent.frame()) {
  force(env)
  for(wfn in lwfn) {
    arg <- applyto(wfn = wfn, arg = arg, env = env)
  }
  arg
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
apply_right.wrapr_funobj_S3 <- function(pipe_left_arg,
                                        pipe_right_arg,
                                        pipe_environment,
                                        left_arg_name,
                                        pipe_string,
                                        right_arg_name) {
  force(pipe_environment)
  applyto(wfn = pipe_right_arg, arg = pipe_left_arg, env = pipe_environment)
}






setOldClass("wrapr_funobj_S3")

#' Define a S4 right action class.
#'
#' @param Class character, name of class.
#' @param where environment to work in.
#'
#' @export
#'
def_funobj_s4_class <- function(Class,
                                where = topenv(parent.frame())) {
  force(Class)
  force(where)
  setClass(Class, slots = c(wfn = "wrapr_funobj_S3"), where = where)
}



#' Set the S4 right action.
#'
#' Set the S4 right action of apply_right_S4 for left_class and right_class.
#'
#' @param left_class character, name of left class.
#' @param right_class character, name of right class.
#' @param where environment to work in.
#'
#' @keywords internal
#'
#' @export
set_funobj_s4_applyto <- function(left_class,
                                  right_class,
                                  where = topenv(parent.frame())) {
  force(left_class)
  force(right_class)
  force(where)
  setMethod(
    "apply_right_S4",
    signature(left_class, right_class),
    function(pipe_left_arg,
             pipe_right_arg,
             pipe_environment,
             left_arg_name,
             pipe_string,
             right_arg_name) {
      force(pipe_environment)
      applyto(wfn = pipe_right_arg, arg = pipe_left_arg, env = pipe_environment)
    },
    where = where)
}



#' Wrap a function and arguments in an object for later S4 pipe dispatch.
#'
#' @param Class character name of class.
#' @param fn function to wrap
#' @param ... force later arguments to be taken by name.
#' @param arg_name name for remaining argument.
#' @param args list of function argument values
#' @return wrapped function
#'
#' @seealso \code{\link{applyto}}
#'
#' @export
#'
wrap_function_S4 <- function(Class, fn,
                             ...,
                             arg_name = '', args = list()) {
  stop_if_dot_args(substitute(list(...)), "wrapr::wrap_function_S4")
  new(Class,
      wfn = wrap_function_S3(fn = fn, arg_name = arg_name, args = args))
}

#' Wrap a function name and arguments in an object for later S4 pipe dispatch.
#'
#' @param Class character name of class.
#' @param fn_name character, name of function.
#' @param ... force later arguments to be taken by name.
#' @param fn_package character, name of package.
#' @param arg_name name for remaining argument.
#' @param args list of function argument values
#' @return wrapped function
#'
#' @seealso \code{\link{applyto}}
#'
#' @export
#'
wrap_fname_S4 <- function(Class, fn_name = NULL,
                          ...,
                          fn_package = "base",
                          arg_name = '', args = list()) {
  stop_if_dot_args(substitute(list(...)), "wrapr::wrap_fname_S4")
  new(Class,
      wfn = wrap_fname_S3(fn_name = fn_name, fn_package = fn_package,
                          arg_name = arg_name, args = args))
}


#' Construct a list of class pipe_list
#'
#' @param ... items to keep, must all be function object classes.
#' @return list of class pipe_list
#'
#' @export
#'
pipe_list <- function(...) {
  r <- list(...)
  for(ri in r) {
    ci <- get_wrapr_funobj_S3_portion(ri)
    if(!("wrapr_funobj_S3" %in% class(ci))) {
      stop("wrapr::pipe_list elements must be of class wrapr_funobj_S3 or a class derived from def_funobj_s4_class()")
    }
  }
  class(r) <- "pipe_list"
  r
}

#' Apply right to a list of items.
#'
#' @param pipe_left_arg left argument
#' @param pipe_right_arg a pipe_list
#' @param pipe_environment environment to evaluate in
#' @param left_arg_name name, if not NULL name of left argument.
#' @param pipe_string character, name of pipe operator.
#' @param right_arg_name name, if not NULL name of right argument.
#' @return result
#'
#' @keywords internal
#'
#' @export
apply_right.pipe_list <- function(pipe_left_arg,
                                  pipe_right_arg,
                                  pipe_environment,
                                  left_arg_name,
                                  pipe_string,
                                  right_arg_name) {
  force(pipe_environment)
  lapplyto(pipe_right_arg, pipe_left_arg, pipe_environment)
}
