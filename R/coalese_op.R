


#' Coalesce values (NULL/NA on left replaced by values on the right).
#'
#' This is a simple "try to take values on the left, but fall back
#' to the right if they are not available" operator.  It is inspired
#' by SQL coalesce and the notation is designed to
#' evoke the idea of testing and the \code{C#} \code{??} null coalescing operator.
#' \code{NA} and \code{NULL} are treated roughly equally: both are
#' replaced regardless of available replacement value (with some exceptions).
#' The exceptions are: if the left hand side is a non-zero length vector
#' we preserve the vector type of the left-hand side and do not assign
#' any values that vectors can not hold (NULLs and complex structures) and do not
#' replace with a right argument list.
#'
#' This operator represents a compromise between the desire to replace
#' length zero structures and NULL/NA values and the desire to preserve
#' the first argument's structure (vector versus list).  The order of
#' operations has been chosen to be safe, convenient, and useful.  Length zero
#' lists are not treated as NULL (which is consistent with R in general).
#' Note for non-vector operations on conditions we recommend looking into
#' \code{\link[base]{isTRUE}}, which solves some problems even faster
#' than coalesce style operators.
#'
#' When length(coalesce_left_arg)<=0 then
#' return coalesce_right_arg if length(coalesce_right_arg)>0, otherwise
#' return coalesce_left_arg.
#' When length(coalesce_left_arg)>0:
#' assume coalesce_left_arg is a list or vector and coalesce_right_arg
#' is a list or vector that is either the same length as coalesce_left_arg
#' or length 1.  In this case replace NA/NULL elements of coalesce_left_arg
#' with corresponding elements of coalesce_right_arg (re-cycling coalesce_right_arg
#' when it is length 1).
#'
#' @param coalesce_left_arg vector or list.
#' @param coalesce_right_arg vector or list.
#' @return coalesce_left_arg with NA elements replaced.
#'
#' @examples
#'
#' c(NA, NA, NA) %?% 5            # returns c(5, 5, 5)
#' c(1, NA, NA) %?% list(5)       # returns c(1, 5, 5)
#' c(1, NA, NA) %?% list(list(5)) # returns c(1, NA, NA)
#' c(1, NA, NA) %?% c(NA, 20, NA) # returns c(1, 20, NA)
#' NULL %?% list()    # returns NULL
#' NULL %?% c(1, NA) # returns c(1, NA)
#' list(1, NULL, NULL) %?% c(3, 4, NA)                         # returns list(1, 4, NA_real_)
#' list(1, NULL, NULL, NA, NA) %?% list(2, NULL, NA, NULL, NA) # returns list(1, NULL, NA, NULL, NA)
#' c(1, NA, NA) %?% list(1, 2, list(3)) # returns c(1, 2, NA)
#' c(1, NA) %?% list(1, NULL)           # returns c(1, NA)
#' # list() %?% list(1, NA, NULL) # throws an error.
#' c() %?% list(1, NA, NULL)    # returns list(1, NA, NULL)
#' c() %?% c(1, NA, 2)          # returns c(1, NA, 2)
#'
#' @export
#'
coalesce <- function(coalesce_left_arg, coalesce_right_arg) {
  # avoid touching coalesce_right_arg if we can
  replace <- NULL
  nl <- length(coalesce_left_arg)
  if(nl>0) {
    replace <- is.na(coalesce_left_arg) |
      vapply(coalesce_left_arg, is.null, logical(1))
    if(!any(replace)) {
      return(coalesce_left_arg)
    }
  } else {
    # zero length, prefer self unless coalesce_right_arg is non-trivial.
    if(length(coalesce_right_arg)>0) {
      if(!is.null(coalesce_left_arg)) {
        stop("wrapr::`%?%` (coalesce)` attempting to replace values of a NULL argument from values in a non-length zero structure")
      }
      return(coalesce_right_arg)
    } else {
      return(coalesce_left_arg)
    }
  }
  # now know nl>0 and some replacement is desired
  nr <- length(coalesce_right_arg)
  if(!(nr %in% c(1, nl))) {
    stop("wrapr::`%?%` (coalesce)` right argument must be same length as left or length 1")
  }
  # now know nl>0, nr = 1 or nl, and some replacement is desired.
  if(is.list(coalesce_left_arg)) {
    # treat left as a list, can hold NULLs.
    if(nr==nl) {
      # logical vector list assign
      coalesce_left_arg[replace] <- coalesce_right_arg[replace]
    } else {
      # logical vector assign of scalar
      coalesce_left_arg[replace] <- coalesce_right_arg[[1]]
    }
  } else {
    # treat left as a vector, can not hold NULLs or complex types
    if(nr==nl) {
      for(i in seq_len(nl)) {
        if(replace[[i]]) {
          v <- coalesce_right_arg[[i]]
          if(is.atomic(v) && length(v)==1) {
            coalesce_left_arg[[i]] <- v
          }
        }
      }
    } else {
      # logical vector assing of scalar
      v <- coalesce_right_arg[[1]]
      if(is.atomic(v) && length(v)==1) {
        coalesce_left_arg[replace] <- v
      }
    }
  }
  coalesce_left_arg
}


#' @describeIn coalesce coalesce operator
#' @export
`%?%` <- function(coalesce_left_arg, coalesce_right_arg) {
  coalesce(coalesce_left_arg, coalesce_right_arg)
}
