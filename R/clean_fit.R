



#' Fit a stats::lm without carying back large structures.
#'
#' @param outcome character, name of outcome column.
#' @param variables character, names of varaible columns.
#' @param data data.frame, training data.
#' @param ... not used, force later arguments to be used by name
#' @param weights passed to stats::glm()
#' @param env environment to work in.
#' @return list(model=model, summary=summary)
#'
#' @examples
#'
#' k = 1000
#' dlarge <- data.frame(
#'   x1 = rep(c("a", "a", "b", "b"), k),
#'   x2 = rep(c(0, 0, 0, 1), k),
#'   y = rep(1:4, k),
#'   yC = rep(c(FALSE, TRUE, TRUE, TRUE), k),
#'   stringsAsFactors = FALSE)
#' dsmall <- data.frame(
#'   x1 = c("a", "a", "b", "b"),
#'   x2 = c(0, 0, 0, 1),
#'   y = 1:4,
#'   yC = c(FALSE, TRUE, TRUE, TRUE),
#'   stringsAsFactors = FALSE)
#' res_lm <- clean_fit_lm("y", c("x1", "x2"), dlarge)
#' str(res_lm$model)
#' predict(res_lm$model, newdata = dsmall)
#'
#' @export
#'
clean_fit_lm <- function(outcome, variables, data,
                         ...,
                         weights = NULL,
                         env = baseenv()) {
  force(env)
  wrapr::stop_if_dot_args(substitute(list(...)), "wrapr::clean_fit_lm")
  if(is.null(weights)) {
    weights <- numeric(nrow(data)) + 1
  }
  # work around this issue:
  # http://www.win-vector.com/blog/2018/12/very-non-standard-calling-in-r/
  wenv <- new.env(parent = env)
  assign("weights", weights, envir = wenv)
  f <- wrapr::mk_formula(outcome, variables, env = wenv)
  m <- stats::lm(f, data,
                 weights = weights,
                 model = FALSE, x = FALSE, y = FALSE)
  s <- stats::summary.lm(m)
  # take out large structures
  m$qr$qr <- NULL
  m$residuals <- NULL
  m$effects <- NULL
  m$fitted.values <- NULL
  m$call <- NULL
  m$model <- NULL
  m$weights <- NULL
  environment(m$terms) <- env
  list(model = m, summary = s)
}


#' Fit a stats::glm without carying back large structures.
#'
#' @param outcome character, name of outcome column.
#' @param variables character, names of varaible columns.
#' @param data data.frame, training data.
#' @param ... not used, force later arguments to be used by name
#' @param family passed to stats::glm()
#' @param weights passed to stats::glm()
#' @param env environment to work in.
#' @return list(model=model, summary=summary)
#'
#' @examples
#'
#' k = 1000
#' dlarge <- data.frame(
#'   x1 = rep(c("a", "a", "b", "b"), k),
#'   x2 = rep(c(0, 0, 0, 1), k),
#'   y = rep(1:4, k),
#'   yC = rep(c(FALSE, TRUE, TRUE, TRUE), k),
#'   stringsAsFactors = FALSE)
#' dsmall <- data.frame(
#'   x1 = c("a", "a", "b", "b"),
#'   x2 = c(0, 0, 0, 1),
#'   y = 1:4,
#'   yC = c(FALSE, TRUE, TRUE, TRUE),
#'   stringsAsFactors = FALSE)
#' res_glm <- clean_fit_glm("yC", c("x1", "x2"), dlarge, family = binomial)
#' str(res_glm$model)
#' predict(res_glm$model, newdata = dsmall, type = "response")
#'
#' @export
#'
clean_fit_glm <- function(outcome, variables, data,
                          ...,
                          family,
                          weights = NULL,
                          env = baseenv()) {
  force(env)
  force(family)
  wrapr::stop_if_dot_args(substitute(list(...)), "wrapr::clean_fit_glm")
  if(is.null(weights)) {
    weights <- numeric(nrow(data)) + 1
  }
  # work around this issue:
  # http://www.win-vector.com/blog/2018/12/very-non-standard-calling-in-r/
  wenv <- new.env(parent = env)
  assign("weights", weights, envir = wenv)
  f <- wrapr::mk_formula(outcome, variables, env = wenv)
  m <- stats::glm(f, data,
                  family = family,
                  weights = weights)
  s <- stats::summary.glm(m)
  # take out large structures
  m$qr$qr <- NULL
  m$residuals <- NULL
  m$effects <- NULL
  m$fitted.values <- NULL
  m$call <- NULL
  m$linear.predictors <- NULL
  m$weights <- NULL
  m$prior.weights <- NULL
  m$data <- NULL
  m$model <- NULL
  m$y <- NULL
  environment(m$terms) <- env
  list(model = m, summary = s)
}



