

#' Argument constructor
#'
#' Build an Argument class from an object.
#'
#' @param x object to wrap
#' @return x wrapped in an Argument container.
#'
#' @export
#'
as_argument <- function(x) {
  UseMethod("as_argument", x)
}

#' Argument constructor
#'
#' Build an Argument class from an object.
#'
#' @param x object to wrap
#' @return x wrapped in an Argument container.
#'
#' @export
#'
as_argument.default <- function(x) {
  r <- list(x)
  class(r) <- "Argument"
  r
}

#' Argument constructor
#'
#' Pass through unaltered.
#'
#' @param x object to wrap
#' @return x wrapped in an Argument container.
#'
#' @export
#'
as_argument.Argument <- function(x) {
  x
}

#' Convert from Argument to value.
#'
#' @param x Argument or general object
#' @return x object
#'
#' @export
#'
as_value <- function(x) {
  UseMethod("as_value", x)
}


#' Default Argument value (self).
#'
#' @param x object
#' @return x
#'
#' @export
#'
as_value.default <- function(x) {
  x
}

#' Argument value (extract).
#'
#' @param x Argument class
#' @return x[[1]]
#'
#' @export
#'
as_value.Argument <- function(x) {
  x[[1]]
}


#' "Argument" apply_left.
#'
#' \code{apply_left} for \code{Argument} \code{class}.  Forced evaluation of
#' right argument and appies \code{apply_right}.
#'
#' @param pipe_left_arg left argument
#' @param pipe_right_arg substitute(pipe_right_arg) argument
#' @param pipe_environment environment to evaluate in
#' @param left_arg_name name, if not NULL name of left argument.
#' @param pipe_string character, name of pipe operator.
#' @param right_arg_name name, if not NULL name of right argument.
#' @return result
#'
#' @examples
#'
#' as_argument(1:3) %.>% wrapfn(sin, "x") %.>% wrapfn(cos, "x") %.>% as_value(.)
#'
#' @export
#'
apply_left.Argument <-  function(pipe_left_arg,
                                 pipe_right_arg,
                                 pipe_environment,
                                 left_arg_name,
                                 pipe_string,
                                 right_arg_name) {
  force(pipe_environment)
  pipe_left_arg <- as_value(pipe_left_arg)
  if(is.call(pipe_right_arg) && (as.character(pipe_right_arg[[1]])=="as_value")) {
    return(apply_left(pipe_left_arg = pipe_left_arg,
                      pipe_right_arg = pipe_right_arg,
                      pipe_environment = pipe_environment,
                      left_arg_name = left_arg_name,
                      pipe_string = pipe_string,
                      right_arg_name = right_arg_name))
  }
  pipe_right_arg <- eval(pipe_right_arg,
                         envir = pipe_environment,
                         enclos = pipe_environment)
  res <- apply_right(pipe_left_arg = pipe_left_arg,
                     pipe_right_arg = pipe_right_arg,
                     pipe_environment = pipe_environment,
                     left_arg_name = left_arg_name,
                     pipe_string = pipe_string,
                     right_arg_name = right_arg_name)
  as_argument(res)
}



