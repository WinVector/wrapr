




#' Check for duplicate rows.
#'
#' Check a simple data.frame (no list or exotic rows) for duplicate rows.
#'
#' @param data data.frame
#' @return TRUE if there are no duplicate rows, else FALSE.
#'
#' @export
#'
has_no_dup_rows <- function(data) {
  if(!is.data.frame(data)) {
    stop("wrapr::has_no_dup_rows(data) data must be a data.frame")
  }
  ndata <- nrow(data)
  if(ndata<=1) {
    return(TRUE)
  }
  keyColNames <- colnames(data)
  nkey <- length(keyColNames)
  if(nkey<=0) {
    return(FALSE)
  }
  ## radix errors out in knitr situations https://github.com/WinVector/wrapr/issues/9
  # idxs <- do.call(order, c(as.list(data), list(method = "radix")))
  idxs <- do.call(order, as.list(data))
  data <- data[idxs, , drop = FALSE]
  rownames(data) <- NULL
  new_value <- c(TRUE, rep(FALSE, ndata-1))
  for(ki in keyColNames) {
    cA <- data[[ki]][-1]
    cB <- data[[ki]][-ndata]
    cAn <- is.na(cA)
    cBn <- is.na(cB)
    check <- ifelse(cAn | cBn, cAn != cBn, cA != cB)
    di <- c(TRUE, check)
    new_value <- new_value | di
  }
  return(isTRUE(all(new_value)))
}
# # timings back when we were using radix
# n <- 1000
# set.seed(2352)
#
# d1 <- data.frame(x1 = sample(letters, n, replace = TRUE),
#                  x2 = sample(letters, n, replace = TRUE),
#                  x3 = sample(letters, n, replace = TRUE),
#                  x4 = sample(letters, n, replace = TRUE))
# d1_decision <- anyDuplicated(d1)<=0
# d2 <- d1
# while((anyDuplicated(d2)<=0)==d1_decision) {
#   d2 <- data.frame(x1 = sample(letters, n, replace = TRUE),
#                    x2 = sample(letters, n, replace = TRUE),
#                    x3 = sample(letters, n, replace = TRUE),
#                    x4 = sample(letters, n, replace = TRUE))
# }
#
# my_check <- function(values) {
#   all(sapply(values[-1], function(x) identical(values[[1]], x)))
# }
#
# print(anyDuplicated(d1)<=0)
# microbenchmark::microbenchmark(
#   any_dup = { anyDuplicated(d1)<=0 },
#   has_no_dup = { wrapr::has_no_dup_rows(d1) },
#   check = my_check
# )
# # Unit: microseconds
# # expr       min         lq      mean   median        uq       max neval cld
# # any_dup 16528.260 21851.0965 31134.210 30628.08 39116.609 96021.977   100   b
# # has_no_dup   728.621   900.9505  1031.029   964.23  1068.575  4182.615   100  a
#
#
# print(anyDuplicated(d2)<=0)
# microbenchmark::microbenchmark(
#   any_dup = { anyDuplicated(d2)<=0 },
#   has_no_dup = { wrapr::has_no_dup_rows(d2) },
#   check = my_check
# )
# # Unit: microseconds
# # expr       min       lq       mean     median       uq       max neval cld
# # any_dup 16679.607 17874.35 20899.9830 19369.3610 21756.95 50894.236   100   b
# # has_no_dup   735.003   859.79   973.4994   927.8125  1053.39  2099.416   100  a


#' Check that a set of columns form unique keys.
#'
#' For local data.frame only.
#'
#' @param data data.frame to work with.
#' @param keyColNames character array of column names to check.
#' @return logical TRUE if the rows of data are unique addressable by the columns named in keyColNames.
#'
#'
#' @examples
#'
#' d <- data.frame(key = c('a','a', 'b'), k2 = c(1 ,2, 2))
#' checkColsFormUniqueKeys(d, 'key') # should be FALSE
#' checkColsFormUniqueKeys(d, c('key', 'k2')) # should be TRUE
#'
#' @export
#'
checkColsFormUniqueKeys <- function(data, keyColNames) {
  if(!is.data.frame(data)) {
    stop("wrapr::checkColsFormUniqueKeys data should be a data.frame")
  }
  if(length(keyColNames)!=length(unique(keyColNames, allowNAKeys=TRUE))) {
    stop("wrapr::checkColsFormUniqueKeys keyColNames must not have duplicates/NAs")
  }
  cn <- colnames(data)
  if(length(setdiff(keyColNames, cn))>0) {
    stop("wrapr::checkColsFormUniqueKeys all keyColNames must be columns of data")
  }
  # count the number of rows
  ndata <- nrow(data)
  if(ndata<=1) {
    return(TRUE)
  }
  if(length(keyColNames) <= 0) {
    return(FALSE)
  }
  data <- data[, keyColNames, drop = FALSE]
  rownames(data) <- NULL
  # identify duplicate rows, no duplicated is the obvious way, the
  # code below is an attempt at a speedup (at the cost of space).
  # return(anyDuplicated(data)<=0)
  return(has_no_dup_rows(data))
}




#' Check two data.frames are equivalent after sorting columns and rows.
#'
#' Confirm two dataframes are equivalent after reordering columns and rows.
#'
#' @param d1 data.frame 1
#' @param d2 data.frame 2
#' @param ... force later arguments to bind by name
#' @param tolerance numeric comparision tolerance
#' @return logical TRUE if equivalent
#'
#' @export
#'
check_equiv_frames <- function(d1, d2,
                               ...,
                               tolerance = sqrt(.Machine$double.eps)) {
  wrapr::stop_if_dot_args(substitute(list(...)), "wrapr::check_equiv_frames")
  if( (!is.data.frame(d1)) != (!is.data.frame(d2)) ) {
    return(FALSE)
  }
  d1 <- data.frame(d1)
  d2 <- data.frame(d2)
  if((nrow(d1)!=nrow(d2)) || (ncol(d1)!=ncol(d2))) {
    return(FALSE)
  }
  cols <- sort(colnames(d1))
  c2 <- sort(colnames(d2))
  if(!isTRUE(all.equal(cols, c2))) {
    return(FALSE)
  }
  d1 <- d1[, cols, drop=FALSE]
  d1 <- d1[orderv(d1), , drop=FALSE]
  rownames(d1) <- NULL
  d2 <- d2[, cols, drop=FALSE]
  d2 <- d2[orderv(d2), , drop=FALSE]
  rownames(d2) <- NULL
  for(c in cols) {
    c1 <- d1[[c]]
    c2 <- d2[[c]]
    if(is.numeric(c1) != is.numeric(c2)) {
      return(FALSE)
    }
    if(is.numeric(c1)) {
      if(!isTRUE(all.equal(c1, c2, tolerance=tolerance))) {
        return(FALSE)
      }
    } else {
      if(!isTRUE(all.equal(c1, c2))) {
        return(FALSE)
      }
    }
  }
  return(TRUE)
}
