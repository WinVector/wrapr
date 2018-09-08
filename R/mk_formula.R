
#' @importFrom stats update.formula
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
#'
#' @param outcome character scalar, name of outcome or dependent variable.
#' @param variables character vector, names of input or independent variables.
#' @param ... not used, force later arguments to bind by name.
#' @param env environment to use in formula.
#' @return a formula object
#'
#' @seealso \code{\link[stats]{update.formula}}
#'
#' @examples
#'
#' mk_formula("mpg", c("cyl", "disp"))
#'
#' @export
#'
mk_formula <- function(outcome, variables,
                       ...,
                       env = parent.frame()) {
  wrapr::stop_if_dot_args(substitute(list(...)), "wrapr::mk_formula")
  if((!is.character(outcome)) || (length(outcome)!=1)) {
    stop("wrapr::mk_formula outcome must be a length 1 character vector")
  }
  if((!is.character(variables)) || (length(variables)<1)) {
    stop("wrapr::mk_formula variables must be a length 1 or greater character vector")
  }
  f <- do.call(
    "~",
    list(as.name(outcome),
         as.name(variables[[1]])),
    envir = env)
  fs <- c(list(f),
          lapply(
            variables[-1],
            function(vi) {
              do.call(
                "~",
                list(as.name(outcome),
                     call("+", as.name("."), as.name(vi))),
                envir = env)
            }
          ))
  Reduce(update.formula, fs)
}
