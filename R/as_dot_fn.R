

#' Convert an unevaluted pipeline into a function of "."
#'
#' Convert an unevaluted pipeline into a function.
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
    assign(".", ., envir = env)
    eval(pipeline, envir = env, enclos = env)
  }
  f_env = new.env(parent = env)
  assign("pipeline", pipeline, envir = env)
  environment(f) <- f_env
  f
}
