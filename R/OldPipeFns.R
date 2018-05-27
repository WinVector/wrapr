
# Old pipe functions to support rquery 0.4.3.  Remove these after rquery advances in version.
# Out of date, use \code{\link{apply_left}} and \code{\link{apply_rigth}} instead.


#' pipe_step
#'
#' Out of date, please use \code{\link{apply_left}} instead.
#'
#' @param pipe_left_arg left argument.
#' @param pipe_right_arg substitute(pipe_right_arg) argument.
#' @param pipe_environment environment to evaluate in.
#' @param pipe_name character, name of pipe operator.
#' @return result
#'
#' @export
#'
pipe_step <- function(pipe_left_arg,
                      pipe_right_arg,
                      pipe_environment,
                      pipe_name = NULL) {
  UseMethod("pipe_step", pipe_left_arg)
}


#' pipe_step
#'
#' Out of date, please use \code{\link{apply_left}} instead.
#'
#' @param pipe_left_arg left argument.
#' @param pipe_right_arg substitute(pipe_right_arg) argument.
#' @param pipe_environment environment to evaluate in.
#' @param pipe_name character, name of pipe operator.
#' @return result
#'
#' @export
#'
pipe_step.default <- function(pipe_left_arg,
                              pipe_right_arg,
                              pipe_environment,
                              pipe_name = NULL) {
  apply_left(pipe_left_arg = pipe_left_arg,
             pipe_right_arg = pipe_right_arg,
             pipe_environment = pipe_environment,
             left_arg_name = NULL,
             pipe_string = pipe_name,
             right_arg_name= NULL)
}


#' wrapr_function
#'
#' Out of date, please use \code{\link{apply_right}} instead.
#'
#' @param pipe_left_arg left argument.
#' @param pipe_right_arg right argument (general object, not a function).
#' @param pipe_environment environment to evaluate in.
#' @param pipe_name character, name of pipe operator.
#' @return result
#'
#' @export
#'
wrapr_function <- function(pipe_left_arg,
                           pipe_right_arg,
                           pipe_environment,
                           pipe_name = NULL) {
  UseMethod("wrapr_function", pipe_right_arg)
}

#' wrapr_function
#'
#' Out of date, please use \code{\link{apply_right}} instead.
#'
#' @param pipe_left_arg left argument.
#' @param pipe_right_arg substitute(pipe_right_arg) argument.
#' @param pipe_environment environment to evaluate in.
#' @param pipe_name character, name of pipe operator.
#' @return result
#'
#' @export
#'
wrapr_function.default <- function(pipe_left_arg,
                                   pipe_right_arg,
                                   pipe_environment,
                                   pipe_name = NULL) {
  # go to default left S3 dispatch on pipe_step()
  apply_left(pipe_left_arg = pipe_left_arg,
             pipe_right_arg = pipe_right_arg,
             pipe_environment = pipe_environment,
             left_arg_name = NULL,
             pipe_string = pipe_name,
             right_arg_name= NULL)
}

