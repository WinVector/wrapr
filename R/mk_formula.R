
#' @importFrom stats update.formula lm
NULL

# build a sum as pluses for formula interface.
r_plus <- function(vars, add_zero = FALSE) {
  res <- NULL
  if(add_zero) {
    res <- 0
  }
  nv <- length(vars)
  if(nv<1) {
    if(is.null(res)) {
      res <- 1
    }
    return(res)
  }
  if(is.null(res)) {
    firsti <- 2
    res <- as.name(vars[[1]])
  } else {
    firsti <- 1
  }
  for(i in seqi(firsti, nv)) {
    res <- call("+", res, as.name(vars[[i]]))
  }
  res
}

#' Construct a formula.
#'
#' Safely construct a simple Wilkinson notation formula from the outcome (dependent variable) name
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
#' Care must be taken with later arguments to functions like \code{lm()} whose help states:
#' "All of weights, subset and offset are evaluated in the same way as variables in formula, that is first in data and then in the environment of formula."
#' Also note \code{env} defaults to \code{baseenv()} to try and minimize refence leaks produced by the environemnt
#' captured by the formal ending up stored in the resulting model for \code{lm()} and \code{glm()}.  For
#' behavior closer to \code{as.formula()} please set the \code{env} argument to \code{parent.frame()}.
#'
#' @param outcome character scalar, name of outcome or dependent variable.
#' @param variables character vector, names of input or independent variables.
#' @param ... not used, force later arguments to bind by name.
#' @param intercept logical, if TRUE allow an intercept term.
#' @param outcome_target scalar, if not NULL write outcome==outcome_target in formula.
#' @param outcome_comparator one of "==", "!=", ">=", "<=", ">", "<", only use of outcome_target is not NULL.
#' @param env environment to use in formula (unless extra_values is non empty, then this is a parent environemnt).
#' @param extra_values if not empty extra values to be added to a new formula environment containing env.
#' @return a formula object
#'
#' @seealso \code{\link[stats]{reformulate}}, \code{\link[stats]{update.formula}}
#'
#' @examples
#'
#' f <- mk_formula("mpg", c("cyl", "disp"))
#' print(f)
#' (model <- lm(f, mtcars))
#' format(model$terms)
#'
#' f <- mk_formula("cyl", c("wt", "gear"), outcome_target = 8, outcome_comparator = ">=")
#' print(f)
#' (model <- glm(f, mtcars, family = binomial))
#' format(model$terms)
#'
#' @export
#'
mk_formula <- function(outcome, variables,
                       ...,
                       intercept = TRUE,
                       outcome_target = NULL,
                       outcome_comparator = "==",
                       env = baseenv(),
                       extra_values = NULL) {
  force(env)
  wrapr::stop_if_dot_args(substitute(list(...)), "wrapr::mk_formula")
  if((!is.character(outcome)) || (length(outcome)!=1)) {
    stop("wrapr::mk_formula outcome must be a length 1 character vector")
  }
  nv <- length(variables)
  if(nv>0) {
    if(!is.character(variables)) {
      stop("wrapr::mk_formula variables must be a character vector")
    }
  }
  outcome_name <- as.name(outcome)
  outcome_expr <- outcome_name
  if(length(extra_values)>0) {
    env <- new.env(parent = env)
    for(ni in names(extra_values)) {
      assign(ni, extra_values[[ni]], envir = env)
    }
  }
  if(!is.null(outcome_target)) {
    if(outcome_comparator=="==") {
      outcome_expr <- bquote((.(outcome_name) == .(outcome_target)))
    } else if(outcome_comparator=="!=") {
        outcome_expr <- bquote((.(outcome_name) != .(outcome_target)))
    } else if(outcome_comparator==">=") {
      outcome_expr <- bquote((.(outcome_name) >= .(outcome_target)))
    } else if(outcome_comparator=="<=") {
      outcome_expr <- bquote((.(outcome_name) <= .(outcome_target)))
    } else if(outcome_comparator==">") {
      outcome_expr <- bquote((.(outcome_name) > .(outcome_target)))
    } else if(outcome_comparator=="<") {
      outcome_expr <- bquote((.(outcome_name) < .(outcome_target)))
    } else {
      stop('wrapr::mk_formula outcome_comparator must be one of "==", "!=", ">=", "<=", ">", "<"')
    }
  }
  if(nv<1) {
    if(!intercept) {
      f <- do.call(
        "~",
        list(outcome_expr,
             0),
        envir = env)
    } else {
      f <- do.call(
        "~",
        list(outcome_expr,
             1),
        envir = env)
    }
    return(f)
  }
  rhs_expr <- r_plus(variables, !intercept)
  f <- do.call(
    "~",
    list(outcome_expr,
         rhs_expr),
    envir = env)
  f
}
