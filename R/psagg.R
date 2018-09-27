

#' Pseudo aggregator.
#'
#' Take a vector or list and return the first element (pseudo-aggregation or projection).
#' If the argument length is zero or there are different items throw in an error.
#'
#' This function is useful in some split by column situations as a safe and legible
#' way to convert vectors to scalars.
#'
#' @param x should be a vector or list of items.
#' @param ... force later arguments to be passed by name
#' @param strict logical, should we check value uniqueness.
#' @return x[[1]] (or throw if not all items are equal or this is an empty vector).
#'
#' @examples
#'
#' d <- data.frame(
#'   group = c("a", "a", "b"),
#'   stringsAsFactors = FALSE)
#' dl <- lapply(
#'   split(d, d$group),
#'   function(di) {
#'     data.frame(
#'       # note: di$group is a possibly length>1 vector!
#'       # pseudo aggregate it to the value that is
#'       # constant for each group, confirming it is constant.
#'       group_label = psagg(di$group),
#'       group_count = nrow(di),
#'       stringsAsFactors = FALSE
#'     )
#'   })
#' do.call(rbind, dl)
#'
#' @export
#'
psagg <- function(x,
                  ...,
                  strict = TRUE) {
  stop_if_dot_args(substitute(list(...)), "wrapr::psagg")
  len <- length(x)
  if(len<1) {
    stop("wrapr::psagg length zero argument")
  }
  v <- x[[1]]
  if(len>1) {
    null_pos <- vapply(x, is.null, logical(1))
    if(any(null_pos)) {
      if(!all(null_pos)) {
        stop("wrapr::psagg argument mix of NULLs and non-NULLs")
      }
      return(v)
    }
    na_pos <- vapply(x, is.na, logical(1))
    if(any(na_pos)) {
      if(!all(na_pos)) {
        stop("wrapr::psagg argument mix of NAs and non-NAs")
      }
      return(v)
    }
    if(strict) {
      if(length(unique(x))!=1) {
        stop("wrapr::psagg argument values are varying")
      }
    }
  }
  v
}

