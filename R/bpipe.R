

#' Pipe-like operator ("dot block pipe").
#'
#' Defined as: \code{a \%.>\% b} roughly ~ \code{\{ . <- a; b \};} (though, we try to stop .-side effects).
#' Please see \url{http://www.win-vector.com/blog/2017/07/in-praise-of-syntactic-sugar/}.
#'
#' @param a left argument (substituted into .)
#' @param b right argument (presumably including .)
#' @return  \{ . <- a; b \}; (with .-side effects removed)
#'
#' @examples
#'
#' cos(exp(sin(4)))
#' 4 %.>% sin(.) %.>% exp(.) %.>% cos(.)
#'
#' @export
`%.>%` <- function(a, b) {
  a <- substitute(a)
  b <- substitute(b)
  e <- parent.frame()
  # force a
  a <- eval(a,
            envir=e,
            enclos=e)
  # check for a previous . (try to avoid visible side-effects)
  hadDot <- FALSE
  prevDot <- NULL
  tryCatch(
    {
      prevDot <- get(".", envir = e, inherits = FALSE)
      hadDot <- TRUE
    },
    error = function(e) { e })
  assign(".", a, envir= e, inherits= FALSE)
  r <- eval(b,
            envir=e,
            enclos=e)
  if(hadDot) {
    assign(".", prevDot, envir= e,  inherits= FALSE)
  } else {
    rm(list=".",  envir= e, inherits= FALSE)
  }
  r
}


