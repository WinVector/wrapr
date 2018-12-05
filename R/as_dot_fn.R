
#' Convert an unevaluted pipeline into a function.
#'
#' Convert an unevaluted pipeline into a function of "."
#'
#' Note: writes "." into env.
#'
#' @param pipeline a un-evaluated wrapr pipeline.
#' @param env envirnonment to work in.
#' @return single function with signature (., env = parent.frame())
#'
#' @examples
#'
#' f <- as_dot_fn(sin(.) %.>% cos(.))
#' f(1:3)
#'
#' g <- as_dot_fn(. %.>% sin(.) %.>% cos(.))
#' g(1:3)
#'
#' @export
#'
as_dot_fn <- function(pipeline, env = parent.frame()) {
  force(env)
  pipeline <- substitute(pipeline)
  f <- function(., env = parent.frame()) {
    force(env)
    assign(".", ., envir = env)
    eval(pipeline, envir = env, enclos = env)
  }
  f_env = new.env(parent = env)
  assign("pipeline", pipeline, envir = env)
  environment(f) <- f_env
  f
}

#' Convert a pipeable object into a function.
#'
#' Convert a pipeable object into a function of "."
#'
#' Note: writes "." into env.
#'
#' @param pipeable a wrapr dot-pipe pipeable object
#' @param env envirnonment to work in.
#' @return single function with signature (., env = parent.frame())
#'
#' @examples
#'
#' p <- pkgfn("base::sin", "x")
#' f <- as_fn(p)
#' f(5)
#'
#'
#' @export
#'
as_fn <- function(pipeable, env = parent.frame()) {
  force(env)
  right_arg_name = "pipeable"
  ps <- substitute(pipeable)
  if(is.name(ps)) {
    right_arg_name <- as.character(substitute(ps))
  }
  f <- function(., env = parent.frame()) {
    force(env)
    left_arg_name = "."
    ds <- substitute(.)
    if(is.name(ds)) {
      left_arg_name <- as.character(ds)
    }
    assign(".", ., envir = env)
    wrapr::apply_right(pipe_left_arg = .,
                       pipe_right_arg = pipeable,
                       pipe_environment = env,
                       left_arg_name = left_arg_name,
                       pipe_string = "%.>%",
                       right_arg_name = right_arg_name)
  }
  f_env = new.env(parent = env)
  assign("pipeable", pipeable, envir = env)
  assign("right_arg_name", right_arg_name, envir = env)
  environment(f) <- f_env
  f
}
