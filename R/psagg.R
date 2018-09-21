

#' Pseudo aggregator.
#'
#' Take a vector or list and return the first element (pseudo-aggregation or projection).
#' If the argument length is zero or there are different items throw in an error.
#'
#' This function is useful in some split by column situations as a safe and legible
#' way to convert vectors to scalars.
#'
#' @param x should be a vector or list of simple (comparable) items.
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
psagg <- function(x) {
  len <- length(x)
  if(len<1) {
    stop("wrapr::psagg empty argument")
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
    comps <- vapply(
      2:len,
      function(i) {
        identical(x[[1]], x[[i]])
      },
      logical(1))
    if(!all(comps)) {
      stop("wrapr::psagg argument values are varying")
    }
  }
  v
}

