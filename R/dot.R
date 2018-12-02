
#' Inline dot product.
#'
#' @param e1 first, or left argument.
#' @param e2 second, or right argument.
#' @return c(e1, c2)
#'
#' @examples
#'
#' c(1,2) %dot% c(3, 5)
#'
#'
#' @rdname inline_dot
#'
#' @export
#'
`%dot%` <- function(e1, e2) {
  sum(e1 * e2)
}
