

#' Inline list/array concatenate.
#'
#' @param e1 first, or left argument.
#' @param e2 second, or right argument.
#' @return c(e1, c2)
#'
#' @examples
#'
#' 1:2 %c% 5:6
#'
#' c("a", "b") %c% "d"
#'
#' @rdname inline_concat
#'
#' @export
#'
`%c%` <- function(e1, e2) {
  c(e1, e2)
}

#' Inline quoting list/array concatenate.
#'
#' @param e1 first, or left argument.
#' @param e2 second, or right argument.
#' @return qc(e1, c2)
#'
#' @examples
#'
#' 1:2 %qc% 5:6
#'
#' c("a", "b") %qc% d
#'
#' a %qc% b %qc% c
#'
#' @rdname inline_qc
#'
#' @export
#'
`%qc%` <- function(e1, e2) {
  env <- parent.frame()
  do.call(qc, list(substitute(e1), substitute(e2)),
          envir = env)
}

