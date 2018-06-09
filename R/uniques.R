

#' Strict version of unique (without ...).
#'
#' Check that \code{...} is empty and if so call
#' \code{base::unique(x, incomparables = incomparables, MARGIN = MARGIN, fromLast = fromLast)}
#' (else throw an error)
#'
#' @param x items to be compared.
#' @param ... not used, checked to be empty to prevent errors.
#' @param incomparables passed to base::unique.
#' @param MARGIN passed to base::unique.
#' @param fromLast passed to base::unique.
#' @return base::unique(x, incomparables = incomparables, MARGIN = MARGIN, fromLast = fromLast)
#'
#'
#' @examples
#'
#' x = c("a", "b")
#' y = c("b", "c")
#'
#' # task: get unique items in x plus y
#' unique(c(x, y))   # correct answer
#' unique(x, y)      # oops forgot to wrap arguments, quitely get wrong answer
#' tryCatch(
#'    uniques(x, y), # uniques catches the error
#'    error = function(e) { e })
#' uniques(c(x, y))  # uniques works like base::unique in most case
#'
#' @export
#'
uniques <- function(x,
                    ...,
                    incomparables = FALSE,
                    MARGIN = 1,
                    fromLast = FALSE) {
  wrapr::stop_if_dot_args(substitute(list(...)), "wrapr::uniques")
  base::unique(x,
               incomparables = incomparables,
               MARGIN = MARGIN,
               fromLast)
}
