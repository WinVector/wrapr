
#' @importFrom utils View head
NULL

#' Invoke a spreadsheet like viewer when appropriate.
#'
#' @param x R object to view
#' @param ... force later arguments to bind by name.
#' @param title title for viewer
#' @param n number of rows to show
#' @return invoke view or format object
#'
#' @examples
#'
#' view(mtcars)
#'
#' @export
#'
view <- function(
  x,
  ...,
  title = wrapr_deparse(substitute(x)),
  n = 200) {
  UseMethod("view", x)
}


#' @export
view.data.frame <- function(
  x,
  ...,
  title = paste(deparse(substitute(x)), collapse = " "),
  n = 200) {
  wrapr::stop_if_dot_args(substitute(list(...)),
                          "view.data.frame")
  if(interactive()) {
    do.call("View", list(x, title=title),
         envir = globalenv())
  } else {
    if(requireNamespace("knitr",
                        quietly = TRUE)) {
      knitr::kable(head(x, n = n),
                   caption = title)
    } else {
      print(format(head(x, n = n)))
    }
  }
}

#' @export
view.default <- function(
  x,
  ...,
  title = paste(deparse(substitute(x)), collapse = " "),
  n = 200) {
  wrapr::stop_if_dot_args(substitute(list(...)),
                          "view.default")
  if(is.data.frame(x)) {
    return(view(as.data.frame(x), title = title, n = n))
  }
  cls <- class(x)
  if((length(cls)==1) &&
     (cls %in% qc(numeric, character, integer, logical))) {
    d <- data.frame(V1 = 1)
    d$V1 <- x
    colnames(d) <- title
    view(d, title = title, n = n)
  } else {
    view(as.data.frame(x), title = title, n = n)
  }
}

