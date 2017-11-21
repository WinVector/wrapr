

#' Quoting version of c() array concatinator.
#'
#' @param ... items to place into an array
#' @return quoted array of character items
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
  args <- substitute(list(...))
  names <- names(args)
  res <- lapply(seq_len(length(args)),
                function(i) {
                  ei <- args[[i]]
                  ni <- NULL
                  if(i<=length(names)) {
                    ni <- as.character(names[[i]])
                  }
                  if(is.name(ei) | is.character(ei)) {
                    if(is.null(ni)) {
                      return(as.character(ei))
                    } else {
                      return(ni := as.character(ei))
                    }
                  }
                  ln = length(ei)
                  if(ln<=0) {
                    return(NULL)
                  }
                  if(ln<=1) {
                    if(is.null(ni)) {
                      return(as.character(ei))
                    } else {
                      return(ni := as.character(ei))
                    }
                  }
                  # complex structure, like a list
                  if(is.language(ei)) {
                    return(eval(ei))
                  }
                  return(ei[2:ln])
                })
  res <- Filter(function(ei) { !is.null(ei) }, res)
  if(length(res)<=1) {
    return(c())
  }
  res <- res[2:length(res)]
  unlist(res)
}

