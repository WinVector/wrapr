

#' Quoting version of c() array concatinator.
#'
#' Note: evaluation of qc() arguments happens in the qc() environment, not the calling environment.
#' This means assigments inside a qc() argument may not be visible, also name collisions with
#' \code{...} or \code{.wrapr_private_var_} prefixed variables are possible.
#'
#' @param ... items to place into an array
#' @return quoted array of character items
#'
#' @seealso \code{\link{qe}}, \code{\link{qae}}
#'
#' @examples
#'
#' qc(a, qc(b, c))
#' qc(x=a, qc(y=b, z=c))
#' qc('x'='a', qc('y'='b', 'z'='c'))
#'
#' @export
#'
qc <- function(...) {
  .wrapr_priveate_var_args <- substitute(list(...))
  if(length(.wrapr_priveate_var_args)<=1) {
    return(c())
  }
  .wrapr_priveate_var_names <- names(.wrapr_priveate_var_args)
  .wrapr_priveate_var_res <- lapply(2:length(.wrapr_priveate_var_args),
                function(.wrapr_priveate_var_i) {
                  .wrapr_priveate_var_ei <- .wrapr_priveate_var_args[[.wrapr_priveate_var_i]]
                  .wrapr_priveate_var_ni <- NULL
                  if(.wrapr_priveate_var_i<=length(.wrapr_priveate_var_names)) {
                    .wrapr_priveate_var_ni <- as.character(.wrapr_priveate_var_names[[.wrapr_priveate_var_i]])
                  }
                  if(is.name(.wrapr_priveate_var_ei) | is.character(.wrapr_priveate_var_ei)) {
                    if(is.null(.wrapr_priveate_var_ni)) {
                      return(as.character(.wrapr_priveate_var_ei))
                    } else {
                      return(.wrapr_priveate_var_ni := as.character(.wrapr_priveate_var_ei))
                    }
                  }
                  .wrapr_priveate_var_ln = length(.wrapr_priveate_var_ei)
                  if(.wrapr_priveate_var_ln<=0) {
                    return(NULL)
                  }
                  if(.wrapr_priveate_var_ln<=1) {
                    if(is.null(.wrapr_priveate_var_ni)) {
                      return(as.character(.wrapr_priveate_var_ei))
                    } else {
                      return(.wrapr_priveate_var_ni := as.character(.wrapr_priveate_var_ei))
                    }
                  }
                  # complex structure, like a list
                  if(is.language(.wrapr_priveate_var_ei)) {
                    return(eval(.wrapr_priveate_var_ei))
                  }
                  return(.wrapr_priveate_var_ei[2:.wrapr_priveate_var_ln])
                })
  .wrapr_priveate_var_res <- Filter(function(ei) { !is.null(ei) }, .wrapr_priveate_var_res)
  if(length(.wrapr_priveate_var_res)<1) {
    return(c())
  }
  unlist(.wrapr_priveate_var_res)
}

