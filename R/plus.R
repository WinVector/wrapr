
#' Inline character paste0.
#'
#' @param e1 first, or left argument.
#' @param e2 second, or right argument.
#' @return c(e1, c2)
#'
#' @examples
#'
#' "a" %p% "b"
#'
#' c("a", "b") %p% "_d"
#'
#' @rdname inline_paste0
#'
#' @export
#'
`%p%` <- function(e1, e2) {
  paste0(e1, e2)
}
