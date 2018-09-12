

#' Return a vector of matches.
#'
#' @param pattern character scalar, pattern to match, passed to \code{\link[base]{grep}}.
#' @param x character vector to match to, passed to \code{\link[base]{grep}}.
#' @param ... not used, forced later arguments to bind by name.
#' @param ignore.case logical, passed to \code{\link[base]{grep}}.
#' @param perl logical, passed to \code{\link[base]{grep}}.
#' @param fixed logical, passed to \code{\link[base]{grep}}.
#' @param useBytes logical, passed \code{\link[base]{grep}}.
#' @param invert passed to \code{\link[base]{grep}}.
#' @return vector of matching values.
#'
#' @seealso \code{\link[base]{grep}}, \code{\link{grepdf}}
#'
#' @examples
#'
#' grepv("x$", c("sox", "xor"))
#'
#' @export
#'
grepv <- function(pattern, x, ..., ignore.case = FALSE, perl = FALSE,
                  fixed = FALSE, useBytes = FALSE, invert = FALSE) {
  stop_if_dot_args(substitute(list(...)), "wrapr::grepv")
  x[base::grep(pattern = pattern,
                x = x, ignore.case = ignore.case, perl = perl,
                fixed = fixed, useBytes = useBytes, invert = invert)]
}