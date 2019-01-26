
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
#' @param outcome character scalar, name of outcome or dependent variable.
#' @param variables character vector, names of input or independent variables.
#' @param ... not used, force later arguments to bind by name.
#' @param intercept logical, if TRUE allow an intercept term.
#' @param outcome_target scalar, if not NULL write outcome==outcome_target in formula.
#' @param outcome_comparator one of "==", "!=", ">=", "<=", ">", "<", only use of outcome_target is not NULL.
#' @param env environment to use in formula.
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
                       env = baseenv()) {
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
