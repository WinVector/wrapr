
#' Stop with message if dot_args is a non-trivial list.
#'
#' Generate a stop with a good error message if the dots argument was a non-trivial list.
#' Useful in writing functions that force named arguments.
#'
#' @param dot_args substitute(list(...)) from another function.
#' @param msg character, optional message to prepend.
#' @return NULL or stop()
#'
#' @examples
#'
#' f <- function(x, ..., inc = 1) {
#'    stop_if_dot_args(substitute(list(...)), "f")
#'    x + inc
#' }
#' f(7)
#' f(7, inc = 2)
#' tryCatch(
#'   f(7, 2),
#'   error = function(e) { print(e) }
#' )
#'
#' @export
#'
stop_if_dot_args <- function(dot_args, msg = "") {
  if(length(dot_args)>1) {
    unams <- names(dot_args)[-1]
    uvals <- as.character(dot_args)[-1]
    saw_blank <- any(nchar(uvals)<=0)
    uvals <- sQuote(uvals)
    unams[is.null(unams)] <- ""
    not_null <- nchar(unams)>0
    unams[not_null] <- paste0(unams[not_null], " = ")
    unams[!not_null] <- ""
    str <- paste(paste0(unams, uvals), collapse = ", ")
    if(saw_blank) {
      str <- paste(str, "(empty arguments can be caused by having exta commas in the function call)")
    }
    stop(paste(msg, "unexpected arguments:", str),
         call. = FALSE)
  }
  NULL
}
