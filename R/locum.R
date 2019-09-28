
#' Build a stand in for a future value to be placed in a pipe.
#'
#' The locum stands in for a value to be specified later in a pipeline.
#' This is similar to a lambda or function abstraction.
#'
#' @return a locum stand-in
#'
#' @examples
#'
#' p <- locum() %.>% sin(.)
#' 5 %.>% p
#'
#' @export
#'
#'
locum <- function() {
  locum <- list(stages = list())
  class(locum) <- 'locum'
  return(locum)
}


#' Format a locum for presentation.
#'
#' @param x locum to be formatted
#' @param ... additional arguments, use "start" to replace initial step presentation
#' @return formatted string
#'
#' @examples
#'
#' p <- locum() %.>% sin(.)
#' format(p, start = 5)
#'
#' @export
#'
format.locum <- function(x, ...) {
  args <- list(...)
  locum <- x
  start_name <- 'locum()'
  if('start' %in% names(args)) {
    start_name <- format(args[['start']])
  }
  stage_strs <- vapply(
    locum$stages,
    function(si) {
      format(si$pipe_right_arg)
    }, character(1))
  stage_strs <- c(list(start_name),
                  stage_strs)
  return(paste(stage_strs,
               collapse = " %.>%\n   "))
}


#' Format a locum for presentation.
#'
#' @param x locum to be formatted
#' @param ... additional arguments, use "start" to replace initial step presentation
#' @return formatted string
#'
#' @examples
#'
#' p <- locum() %.>% sin(.)
#' as.character(p, start = 5)
#'
#' @export
#'
as.character.locum <- function(x, ...) {
  return(format(x, ...))
}


#' Print a locum presentation.
#'
#' @param x locum to be formatted
#' @param ... additional arguments, use "start" to replace initial step presentation
#' @return formatted string
#'
#' @examples
#'
#' p <- locum() %.>% sin(.)
#' print(p, start = 5)
#'
#' @export
#'
print.locum <- function(x, ...) {
  cat(format(x, ...))
}


#' S3 dispatch on class of pipe_left_arg for a locum.
#'
#' For formal documentation please see \url{https://github.com/WinVector/wrapr/blob/master/extras/wrapr_pipe.pdf}.
#'
#' @param pipe_left_arg left argument.
#' @param pipe_right_arg substitute(pipe_right_arg) argument.
#' @param pipe_environment environment to evaluate in.
#' @param left_arg_name name, if not NULL name of left argument.
#' @param pipe_string character, name of pipe operator.
#' @param right_arg_name name, if not NULL name of right argument.
#' @return result
#'
#' @export
#'
apply_left.locum <- function(
  pipe_left_arg,
  pipe_right_arg,
  pipe_environment,
  left_arg_name,
  pipe_string,
  right_arg_name) {
  locum <- pipe_left_arg
  capture <- list(
    pipe_right_arg = force(pipe_right_arg),
    pipe_environment = force(pipe_environment),
    left_arg_name = force(left_arg_name),
    pipe_string = force(pipe_string),
    right_arg_name = force(right_arg_name))
  locum$stages <- c(locum$stages, list(capture))
  return(locum)
}


#' S3 dispatch on class of pipe_right_argument for a locum.
#'
#' Triggered if right hand side of pipe stage was a name that does not resolve to a function.
#' For formal documentation please see \url{https://github.com/WinVector/wrapr/blob/master/extras/wrapr_pipe.pdf}.
#'
#' @param pipe_left_arg left argument
#' @param pipe_right_arg right argument
#' @param pipe_environment environment to evaluate in
#' @param left_arg_name name, if not NULL name of left argument.
#' @param pipe_string character, name of pipe operator.
#' @param right_arg_name name, if not NULL name of right argument.
#' @return result
#'
#' @export
#'
apply_right.locum <- function(
  pipe_left_arg,
  pipe_right_arg,
  pipe_environment,
  left_arg_name,
  pipe_string,
  right_arg_name) {
  force(pipe_left_arg)
  locum <- pipe_right_arg
  for(s in locum$stages) {
    pipe_left_arg <- pipe_impl(
      pipe_left_arg,
      s$pipe_right_arg,
      s$pipe_environment,
      pipe_string = s$pipe_string)
  }
  return(pipe_left_arg)
}


