
#' Grep for column names from a \code{data.frame}
#'
#'
#'
#' @param pattern passed to \code{\link[base]{grep}}
#' @param x data.frame to work with
#' @param ... force later arguments to be passed by name
#' @param ignore.case passed to \code{\link[base]{grep}}
#' @param perl passed to \code{\link[base]{grep}}
#' @param value passed to \code{\link[base]{grep}}
#' @param fixed passed to \code{\link[base]{grep}}
#' @param useBytes passed to \code{\link[base]{grep}}
#' @param invert passed to \code{\link[base]{grep}}
#' @return column names of x matching grep condition.
#'
#' @examples
#'
#'
#' d <- data.frame(xa=1, yb=2)
#'
#' # starts with
#' grepdf('^x', d)
#'
#' # ends with
#' grepdf('b$', d)
#'
#' @export
#'
grepdf <- function(pattern, x,
                   ...,
                   ignore.case = FALSE, perl = FALSE, value = FALSE,
                   fixed = FALSE, useBytes = FALSE, invert = FALSE ) {
  stop_if_dot_args(substitute(list(...)), "wrapr::grepdf")
  nms <- colnames(x)
  nms[base::grep(pattern, nms,
                 ignore.case = ignore.case, perl = perl, value = value,
                 fixed = fixed, useBytes = useBytes, invert = invert)]
}
