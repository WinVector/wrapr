

#' Capture all right-arguments unevaluated.
#'
#' Capture all right-arguments unevaluated, using \code{bquote()-.()} escaping.
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
#'
apply_left.Collector <- function(pipe_left_arg,
                                    pipe_right_arg,
                                    pipe_environment,
                                    left_arg_name,
                                    pipe_string,
                                    right_arg_name) {
  force(pipe_environment)
  cap <- do.call(bquote,
                 list(pipe_right_arg, where = pipe_environment),
                 envir = pipe_environment)
  res <- c(pipe_left_arg, list(cap))
  class(res) <- "Collector"
  res
}


#' Build a collector that can capture all pipe stages to the right.
#'
#' Build a collector that can capture all pipe stages to the right, using
#' \code{bquote()-.()} escaping.
#'
#' @return a Collector list-object.
#'
#' @examples
#'
#' phase <- 0.1
#' Collector() %.>% sin(.) %.>% cos(. + .(phase))
#'
#' @export
Collector <- function() {
  col <- list()
  class(col) <- "Collector"
  col
}


#' Evaluate a sequence of expressions with \code{.}-value substituted in.
#'
#' Evaluate a sequence of expressions with \code{.}-value substituted in.
#'
#' Note: not for steps that intend side-effects or have references to items
#' in non-standard environments.
#'
#' @param . value to pass in.
#' @param dot_seq list of expressions.
#' @param env environment to work in.
#' @return sequential evaluation result.
#'
#' @keywords internal
#'
#' @export
#'
eval_dot_sequence <- function(., dot_seq, env = parent.frame()) {
  force(env)
  eval_env <- new.env(parent = env)
  for(si in dot_seq) {
    assign(".", ., envir = eval_env)
    . <- eval(si, envir = eval_env, enclos = eval_env)
  }
  .
}

#' Convert a sequence of expressions into a function.
#'
#' Convert a sequence of expressions into a function.
#'
#' Note: not for steps that intend side-effects or have references to items
#' in non-standard environments.
#'
#' @param dot_seq list of expressions.
#' @param env environment to work in.
#' @return function with signature (., eval_environment = parent.frame())
#'
#' @examples
#'
#' seq <- Collector() %.>% paste(., "a") %.>% paste(., "b")
#' f <- sequence_as_function(seq)
#' f("x")
#'
#' @export
#'
sequence_as_function <- function(dot_seq, env = parent.frame()) {
  force(env)
  force(dot_seq)
  f <- function(., eval_environment = parent.frame()) {
    force(eval_environment)
    eval_dot_sequence(., dot_seq, env = eval_environment)
  }
  def_env <- new.env(parent = env)
  assign("dot_seq", dot_seq, envir = def_env)
  environment(f) <- def_env
  f
}




