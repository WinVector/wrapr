

#' Capture all right-arguments unevaluated.
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
apply_left.Collector <- function(pipe_left_arg,
                                    pipe_right_arg,
                                    pipe_environment,
                                    left_arg_name,
                                    pipe_string,
                                    right_arg_name) {
  force(pipe_environment)
  res <- c(pipe_left_arg, list(pipe_right_arg))
  class(res) <- "Collector"
  res
}


#' Build a collector that can capture all pipe stages to the right.
#'
#' @return a Collector list-object.
#'
#' @examples
#'
#' Collector() %.>% sin(.) %.>% cos(.)
#'
#' @export
Collector <- function() {
  col <- list()
  class(col) <- "Collector"
  col
}

