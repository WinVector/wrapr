

#' Quoting version of c() array concatinator.
#'
#' @param ... items to place into an array
#' @return quoted array of character items
#'
#' @examples
#'
#' qc(a, qc(b, c))
#'
#' @export
#'
qc <- function(...) {
  res <- lapply(substitute(list(...)),
                function(ei) {
                  if(is.name(ei) | is.character(ei)) {
                    return(as.character(ei))
                  }
                  ln = length(ei)
                  if(ln<=0) {
                    return(NULL)
                  }
                  if(ln<=1) {
                    return(as.character(ei))
                  }
                  return(as.character(ei[2:ln]))
                })
  res <- Filter(function(ei) { !is.null(ei) }, res)
  if(length(res)<=1) {
    return(c())
  }
  res <- res[2:length(res)]
  as.character(unlist(res))
}

