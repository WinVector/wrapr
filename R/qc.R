

#' Quoting version of c() array concatenate.
#'
#' The qc() function is intended to help quote user inputs.
#' It is a convenience function allowing the user to elide
#' excess quotation marks.  It quotes its arguments instead
#' of evaluating them, except in the case of a nested
#' call to qc() itself.  Please see the examples for
#' typical uses both for named and un-named character vectors.
#'
#'
#' @param ... items to place into an array
#' @return quoted array of character items
#'
#' @seealso \code{\link{qe}}, \code{\link{qae}}
#'
#' @examples
#'
#' a <- "x"
#' qc(a) # returns the string "a" (not "x")
#'
#' qc("a") # return the string "a" (not "\"a\"")
#'
#' qc(sin(x))  # returns the string "sin(x)"
#'
#' qc(a, qc(b, c)) # returns c("a", "b", "c")
#'
#' qc(x=a, qc(y=b, z=c)) # returns c(x="a", y="b", z="c")
#'
#' qc('x'='a', wrapr::qc('y'='b', 'z'='c')) # returns c(x="a", y="b", z="c")
#'
#' @export
#'
qc <- function(...) {
  .wrapr_priveate_var_args <- substitute(list(...))
  if(length(.wrapr_priveate_var_args)<=1) {
    return(c())
  }
  .wrapr_priveate_var_env <- parent.frame()
  .wrapr_priveate_var_names <- names(.wrapr_priveate_var_args)
  .wrapr_priveate_var_res <- lapply(
    2:length(.wrapr_priveate_var_args),
    function(.wrapr_priveate_var_i) {
      .wrapr_priveate_var_ei <- .wrapr_priveate_var_args[[.wrapr_priveate_var_i]]
      .wrapr_priveate_var_ni <- NULL
      if(.wrapr_priveate_var_i<=length(.wrapr_priveate_var_names)) {
        .wrapr_priveate_var_ni <- .wrapr_priveate_var_names[[.wrapr_priveate_var_i]]
      }
      if(is.name(.wrapr_priveate_var_ei)) {
        # names are scalars
        .wrapr_priveate_var_ei <- as.character(.wrapr_priveate_var_ei)
        if(!is.null(.wrapr_priveate_var_ni)) {
          names(.wrapr_priveate_var_ei) <- .wrapr_priveate_var_ni
        }
        return(.wrapr_priveate_var_ei)
      }
      if(is.language(.wrapr_priveate_var_ei)) {
        .wrapr_priveate_var_fnname <- deparse(.wrapr_priveate_var_ei[[1]])
        .wrapr_priveate_var_fnname <- gsub("[[:space:]]+", "", .wrapr_priveate_var_fnname)
        if(isTRUE(.wrapr_priveate_var_fnname %in% c("qc", "wrapr::qc"))) {
          # this is the recursive case qc('x'='a', qc('y'='b', 'z'='c'))
          .wrapr_priveate_var_ei <- eval(.wrapr_priveate_var_ei,
                                         envir = .wrapr_priveate_var_env,
                                         enclos = .wrapr_priveate_var_env)
          return(.wrapr_priveate_var_ei)
        }
        # other case: quote expression
        .wrapr_priveate_var_ei <- paste(deparse(.wrapr_priveate_var_ei), collapse = "\n")
        if(!is.null(.wrapr_priveate_var_ni)) {
          names(.wrapr_priveate_var_ei) <- .wrapr_priveate_var_ni
        }
        return(.wrapr_priveate_var_ei)
      }
      if(is.vector(.wrapr_priveate_var_ei) || is.list(.wrapr_priveate_var_ei)) {
        if(length(.wrapr_priveate_var_ei)<=0) {
          return(NULL)
        }
      }
      # base case, character vectors, list, and objects
      .wrapr_priveate_var_ei <- paste(as.character(.wrapr_priveate_var_ei), collapse = " ")
      if(!is.null(.wrapr_priveate_var_ni)) {
        names(.wrapr_priveate_var_ei) <- .wrapr_priveate_var_ni
      }
      return(.wrapr_priveate_var_ei)
    })
  .wrapr_priveate_var_res <- Filter(function(ei) { !is.null(ei) }, .wrapr_priveate_var_res)
  if(length(.wrapr_priveate_var_res)<1) {
    return(c())
  }
  unlist(.wrapr_priveate_var_res)
}

