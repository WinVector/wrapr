

#' Build an anonymous function.
#'
#'
#' @param params formal parameters of function, unbound names.
#' @param body substituted body of function to map arguments into.
#' @param env environment to work in.
#' @return user defined function.
#'
#' @seealso \code{\link{lambda}}, \code{\link{defineLambda}}, \code{\link{named_map_builder}}
#'
#' @examples
#'
#' f <- makeFunction_se(as.name('x'), substitute({x*x}))
#' f(7)
#'
#'
#' g <- makeFunction_se(c(as.name('x'), as.name('y')), substitute({ x + 3*y }))
#' g(1,100)
#'
#'
#' @export
#'
makeFunction_se <- function(params, body, env = parent.frame()) {
  force(env)
  vars <- as.character(params)
  formals <- replicate(length(vars), quote(expr = ))
  names(formals) <- vars
  eval(call('function', as.pairlist(formals), body),
       envir = env,
       enclos = env)
}



#' Build an anonymous function.
#'
#' Mostly just a place-holder so lambda-symbol form has somewhere safe to hang its help entry.
#'
#' @param ... formal parameters of function, unbound names, followed by function body (code/language).
#' @param env environment to work in
#' @return user defined function.
#'
#'
#' @seealso \code{\link{defineLambda}}, \code{\link{makeFunction_se}}, \code{\link{named_map_builder}}
#'
#' @examples
#'
#' #lambda-syntax: lambda(arg [, arg]*, body [, env=env])
#' # also works with lambda character as function name
#' # print(intToUtf8(0x03BB))
#'
#' # example: square numbers
#' sapply(1:4, lambda(x, x^2))
#'
#' # example more than one argument
#' f <- lambda(x, y, x+y)
#' f(2,4)
#'
#'
#' @export
#'
lambda <- function(..., env = parent.frame()) {
  force(env)
  args <- base::substitute(list(...))
  body <- args[[length(args)]]
  args <- args[-length(args)]
  params <- base::lapply(args[-1], base::as.name)
  wrapr::makeFunction_se(params, body, env)
}



#' Define lambda function building function.
#'
#' Use this to place a copy of the lambda-symbol
#' function builder in your workspace.
#'
#' @param envir environment to work in.
#' @param name character, name to assign to (defaults to Greek lambda).
#'
#' @seealso \code{\link{lambda}}, \code{\link{makeFunction_se}}, \code{\link{named_map_builder}}
#'
#' @examples
#'
#' defineLambda()
#' # ls()
#'
#' @export
#'
defineLambda <- function(envir = parent.frame(), name = NULL) {
  force(envir)
  if(is.null(name)) {
    name <- intToUtf8(0x03BB)
  }
  assign(name, wrapr::lambda, envir = envir)
}
