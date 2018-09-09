
#' Order by a list of vectors.
#'
#' Preduce an ordering permutation from a list of vectors.  Essentially a non-\code{...} interface to \code{\link[base]{order}}.
#'
#' @param columns list of atomic columns to order on, can be a \code{data.frame}.
#' @param ... not used, force later arguments to bind by name.
#' @param na.last (passed to \code{\link[base]{order}}) for controlling the treatment of NAs. If TRUE, missing values in the data are put last; if FALSE, they are put first; if NA, they are removed.
#' @param decreasing (passed to \code{\link[base]{order}}) logical. Should the sort order be increasing or decreasing? For the "radix" method, this can be a vector of length equal to the number of arguments in \code{...}. For the other methods, it must be length one.
#' @param method (passed to \code{\link[base]{order}}) the method to be used: partial matches are allowed. The default ("auto") implies "radix" for short numeric vectors, integer vectors, logical vectors and factors. Otherwise, it implies "shell". For details of methods "shell", "quick", and "radix", see the help for \code{\link[base]{sort}}.
#' @return ordering permutation
#'
#'
#' @seealso \code{\link[base]{order}}, \code{\link{sortv}}
#'
#' @examples
#'
#' d <- data.frame(x = c(2, 2, 3, 3, 1, 1), y = 6:1)
#' d[order(d$x, d$y), , drop = FALSE]
#' d[orderv(d), , drop = FALSE]
#'
#' @export
#'
orderv <- function(columns,
                   ...,
                   na.last = TRUE,
                   decreasing = FALSE,
                   method = c("auto", "shell", "radix")) {
  wrapr::stop_if_dot_args(substitute(list(...)), "wrapr::orderv")
  if((!is.list(columns)) || (length(columns)<1)) {
    stop("wrapr::orderv columns must be a list containing at least one atomic vector")
  }
  do.call(base::order, c(
    as.list(columns),
    list(
      na.last = na.last,
      decreasing = decreasing,
      method = method)))
}

#' Sort a data.frame.
#'
#' Sort a data.frame by a set of columns.
#'
#' @param data data.frame to sort.
#' @param colnames column names to sort on.
#' @param ... not used, force later arguments to bind by name.
#' @param na.last (passed to \code{\link[base]{order}}) for controlling the treatment of NAs. If TRUE, missing values in the data are put last; if FALSE, they are put first; if NA, they are removed.
#' @param decreasing (passed to \code{\link[base]{order}}) logical. Should the sort order be increasing or decreasing? For the "radix" method, this can be a vector of length equal to the number of arguments in \code{...}. For the other methods, it must be length one.
#' @param method (passed to \code{\link[base]{order}}) the method to be used: partial matches are allowed. The default ("auto") implies "radix" for short numeric vectors, integer vectors, logical vectors and factors. Otherwise, it implies "shell". For details of methods "shell", "quick", and "radix", see the help for \code{\link[base]{sort}}.
#' @return ordering permutation
#'
#'
#' @seealso \code{\link{orderv}}
#'
#' @examples
#'
#' d <- data.frame(x = c(2, 2, 3, 3, 1, 1), y = 6:1)
#' sortv(d, c("x", "y"))
#'
#' @export
#'
sortv <- function(data,
                  colnames,
                  ...,
                  na.last = TRUE,
                  decreasing = FALSE,
                  method = c("auto", "shell", "radix")) {
  wrapr::stop_if_dot_args(substitute(list(...)), "wrapr::sortv")
  if(!is.data.frame(data)) {
    stop("wrapr::sortv data must be a data.frame")
  }
  if(length(colnames)<1) {
    return(data)
  }
  if(!is.character(colnames)) {
    stop("wrapr::sortv colnames must be of type character")
  }
  bads <- setdiff(colnames, colnames(data))
  if(length(bads)>0) {
    stop(paste("wrapr::sortv data colnames that are not in colnames(data):",
               paste(bads, collapse = ", ")))
  }
  perm <- orderv(as.list(data[, colnames, drop= FALSE]),
                 na.last = na.last,
                 decreasing = decreasing,
                 method = method)
  data[perm, , drop = FALSE]
}