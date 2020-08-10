



#' Fit a stats::lm without carying back large structures.
#'
#' Please see \url{https://win-vector.com/2014/05/30/trimming-the-fat-from-glm-models-in-r/} for discussion.
#'
#' @param outcome character, name of outcome column.
#' @param variables character, names of varaible columns.
#' @param data data.frame, training data.
#' @param ... not used, force later arguments to be used by name
#' @param intercept logical, if TRUE allow an intercept term.
#' @param weights passed to stats::glm()
#' @param env environment to work in.
#' @return list(model=model, summary=summary)
#'
#' @examples
#'
#' mk_data_example <- function(k) {
#'   data.frame(
#'     x1 = rep(c("a", "a", "b", "b"), k),
#'     x2 = rep(c(0, 0, 0, 1), k),
#'     y = rep(1:4, k),
#'     yC = rep(c(FALSE, TRUE, TRUE, TRUE), k),
#'     stringsAsFactors = FALSE)
#' }
#'
#' res_lm <- clean_fit_lm("y", c("x1", "x2"),
#'                        mk_data_example(1))
#' length(serialize(res_lm$model, NULL))
#'
#' res_lm <- clean_fit_lm("y", c("x1", "x2"),
#'                        mk_data_example(10000))
#' length(serialize(res_lm$model, NULL))
#'
#' predict(res_lm$model,
#'         newdata = mk_data_example(1))
#'
#' @export
#'
clean_fit_lm <- function(outcome, variables, data,
                         ...,
                         intercept = TRUE,
                         weights = NULL,
                         env = baseenv()) {
  force(env)
  wrapr::stop_if_dot_args(substitute(list(...)), "wrapr::clean_fit_lm")
  if(is.null(weights)) {
    weights <- numeric(nrow(data)) + 1
  }
  # work around this issue:
  # https://win-vector.com/2018/12/03/very-non-standard-calling-in-r/
  wenv <- new.env(parent = env)
  assign("weights", weights, envir = wenv)
  f <- wrapr::mk_formula(outcome, variables,
                         intercept = intercept,
                         env = wenv)
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
#' Please see \url{https://win-vector.com/2014/05/30/trimming-the-fat-from-glm-models-in-r/} for discussion.
#'
#' @param outcome character, name of outcome column.
#' @param variables character, names of varaible columns.
#' @param data data.frame, training data.
#' @param ... not used, force later arguments to be used by name
#' @param family passed to stats::glm()
#' @param weights passed to stats::glm()
#' @param intercept logical, if TRUE allow an intercept term.
#' @param outcome_target scalar, if not NULL write outcome==outcome_target in formula.
#' @param outcome_comparator one of "==", "!=", ">=", "<=", ">", "<", only use of outcome_target is not NULL.
#' @param env environment to work in.
#' @return list(model=model, summary=summary)
#'
#' @examples
#'
#' mk_data_example <- function(k) {
#'   data.frame(
#'     x1 = rep(c("a", "a", "b", "b"), k),
#'     x2 = rep(c(0, 0, 0, 1), k),
#'     y = rep(1:4, k),
#'     yC = rep(c(FALSE, TRUE, TRUE, TRUE), k),
#'     stringsAsFactors = FALSE)
#' }
#'
#' res_glm <- clean_fit_glm("yC", c("x1", "x2"),
#'                          mk_data_example(1),
#'                          family = binomial)
#' length(serialize(res_glm$model, NULL))
#'
#' res_glm <- clean_fit_glm("yC", c("x1", "x2"),
#'                          mk_data_example(10000),
#'                          family = binomial)
#' length(serialize(res_glm$model, NULL))
#'
#' predict(res_glm$model,
#'         newdata = mk_data_example(1),
#'         type = "response")
#'
#' @export
#'
clean_fit_glm <- function(outcome, variables, data,
                          ...,
                          family,
                          intercept = TRUE,
                          outcome_target = NULL,
                          outcome_comparator = "==",
                          weights = NULL,
                          env = baseenv()) {
  force(env)
  force(family)
  wrapr::stop_if_dot_args(substitute(list(...)), "wrapr::clean_fit_glm")
  if(is.null(weights)) {
    weights <- numeric(nrow(data)) + 1
  }
  # work around this issue:
  # https://win-vector.com/2018/12/03/very-non-standard-calling-in-r/
  wenv <- new.env(parent = env)
  assign("weights", weights, envir = wenv)
  f <- wrapr::mk_formula(outcome, variables,
                         intercept = intercept,
                         outcome_target = outcome_target,
                         outcome_comparator = outcome_comparator,
                         env = wenv)
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
  environment(m$formula) <- env
  list(model = m, summary = s)
}



