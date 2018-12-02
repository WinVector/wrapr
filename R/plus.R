
#' Inline character concatenate.
#'
#' @param e1 first, or left argument.
#' @param e2 second, or gith argument.
#' @return c(e1, c2)
#'
#' @examples
#'
#' "a" %+% "b"
#'
#' c("a", "b") %+% "_d"
#'
#' @rdname inline_paste0
#'
#' @export
#'
`%+%` <- function(e1, e2) {
  paste0(e1, e2)
}
