
#' @importFrom stats update.formula lm
NULL

#' Construct a formula.
#'
#' Safely construct a formula from the outcome (dependent variable) name
#' and vector of input (independent variable) names.
#'
#' Note: outcome and variables
#' are each intended to be simple variable names or column names (or .). They are not
#' intended to specify
#' interactions, I()-terms, transforms, general experessions or other complex formula terms.
#' Essentially the same effect as \code{\link[stats]{reformulate}}, but trying to avoid the
#' \code{paste} currently in \code{\link[stats]{reformulate}} by calling \code{\link[stats]{update.formula}}
#' (which appears to work over terms).
#' Another reasonable way to do this is just \code{paste(outcome, paste(variables, collapse = " + "), sep = " ~ ")}.
#'
#' @param outcome character scalar, name of outcome or dependent variable.
#' @param variables character vector, names of input or independent variables.
#' @param ... not used, force later arguments to bind by name.
#' @param intercept logical, if TRUE allow an intercept term.
#' @param env environment to use in formula.
#' @return a formula object
#'
#' @seealso \code{\link[stats]{reformulate}}, \code{\link[stats]{update.formula}}
#'
#' @examples
#'
#' f <- mk_formula("mpg", c("cyl", "disp"))
#' print(f)
#' lm(f, mtcars)
#'
#' @export
#'
mk_formula <- function(outcome, variables,
                       ...,
                       intercept = TRUE,
                       env = baseenv()) {
  force(env)
  wrapr::stop_if_dot_args(substitute(list(...)), "wrapr::mk_formula")
  if((!is.character(outcome)) || (length(outcome)!=1)) {
    stop("wrapr::mk_formula outcome must be a length 1 character vector")
  }
  if(!is.character(variables)) {
    stop("wrapr::mk_formula variables must be a character vector")
  }
  if(!intercept) {
    f <- do.call(
      "~",
      list(as.name(outcome),
           0),
      envir = env)
  } else {
    f <- do.call(
      "~",
      list(as.name(outcome),
           as.name(variables[[1]])),
      envir = env)
    variables <- variables[-1]
  }
  if(length(variables)>0) {
    fs <- c(list(f),
            lapply(
              variables,
              function(vi) {
                do.call(
                  "~",
                  list(as.name(outcome),
                       call("+", as.name("."), as.name(vi))),
                  envir = env)
              }
            ))
    f <- Reduce(update.formula, fs)
  }
  f
}
