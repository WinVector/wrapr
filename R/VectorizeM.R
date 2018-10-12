

#' Memoizing wrapper for lapply.
#'
#'
#' @seealso \code{\link{VectorizeM}}, \code{\link{vapplym}}
#'
#' @param X list or vector of inputs
#' @param FUN function to apply
#' @param ... additional arguments passed to lapply
#'
#' @examples
#'
#' fs <- function(x) { x <- x[[1]]; print(paste("see", x)); sin(x) }
#' # should only print "see" twice, not 6 times
#' lapplym(c(0, 1, 1, 0, 0, 1), fs)
#'
#' @export
#'
lapplym <- function(X, FUN, ...) {
  UX <- unique(X)
  first_indexes <- match(UX, X)
  all_indexes <- match(X, UX)
  res <- lapply(X[first_indexes], FUN, ...)
  res2 <- res[all_indexes]
  names(res2) <- names(X)
  res2
}

#' Memoizing wrapper for vapply.
#'
#'
#' @seealso \code{\link{VectorizeM}}, \code{\link{lapplym}}
#'
#' @param X list or vector of inputs
#' @param FUN function to apply
#' @param FUN.VALUE type of vector to return
#' @param ... additional arguments passed to lapply
#' @param USE.NAMES passed to vapply
#'
#' @examples
#'
#' fs <- function(x) { x <- x[[1]]; print(paste("see", x)); sin(x) }
#' # should only print "see" twice, not 6 times
#' vapplym(c(0, 1, 1, 0, 0, 1), fs, numeric(1))
#'
#' @export
#'
vapplym <- function(X, FUN, FUN.VALUE, ..., USE.NAMES = TRUE) {
  UX <- unique(X)
  first_indexes <- match(UX, X)
  all_indexes <- match(X, UX)
  res <- vapply(X[first_indexes], FUN, FUN.VALUE, ...,  USE.NAMES = USE.NAMES)
  res2 <- res[all_indexes]
  if(USE.NAMES) {
    names(res2) <- names(X)
  }
  res2
}


#' Memoizing wrapper to base::Vectorize()
#'
#' Build a wrapped function that applies to each unique argument in a vector of arguments once.
#'
#' Only sensible for pure side-effect free deterministic functions.
#'
#' @seealso \code{\link[base]{Vectorize}}, \code{\link{vapplym}}, \code{\link{lapplym}}
#'
#'
#'
#' @param FUN	function to apply, found via match.fun.
#' @param vectorize.args	a character vector of arguments which should be vectorized. Defaults to first argument of FUN.  If set must be length 1.
#' @param SIMPLIFY logical or character string; attempt to reduce the result to a vector, matrix or higher dimensional array; see the simplify argument of sapply.
#' @param USE.NAMES	logical; use names if the first ... argument has names, or if it is a character vector, use that character vector as the names.
#' @param UNLIST logical; if TRUE try to unlist the result.
#'
#' @examples
#'
#' fs <- function(x) { x <- x[[1]]; print(paste("see", x)); sin(x) }
#' fv <- VectorizeM(fs)
#' # should only print "see" twice, not 6 times
#' fv(c(0, 1, 1, 0, 0, 1))
#'
#' @export
#'
VectorizeM <- function(FUN, vectorize.args = arg.names, SIMPLIFY = TRUE, USE.NAMES = TRUE, UNLIST = FALSE) {
  arg.names <- as.list(formals(FUN))
  arg.names[["..."]] <- NULL
  arg.names <- names(arg.names)
  vectorize.args <- as.character(vectorize.args)
  if (!length(vectorize.args))
    return(FUN)
  vectorize.args <- vectorize.args[[1]]
  force(FUN)
  force(SIMPLIFY)
  force(USE.NAMES)
  force(UNLIST)
  if (!all(vectorize.args %in% arg.names))
    stop("must specify names of formal arguments for 'vectorize'")
  collisions <- arg.names %in% c("FUN", "SIMPLIFY", "USE.NAMES", "UNLIST",
                                 "vectorize.args")
  if (any(collisions))
    stop(sQuote("FUN"), " may not have argument(s) named ",
         paste(sQuote(arg.names[collisions]), collapse = ", "))
  if(length(vectorize.args)!=1) {
    stop(sQuote("FUN"), " can only vectorize one argument name")
  }
  rm(list = "arg.names")
  FUNV <- function() {
    args <- lapply(as.list(match.call())[-1L], eval, parent.frame())
    if(length(args)!=1) {
      stop(sQuote("FUN"), "VectorizeM function: saw more than one argument group")
    }
    names <- if (is.null(names(args)))
      character(length(args))
    else names(args)
    dovec <- names %in% vectorize.args
    vargs <- args[dovec]
    unique_values <- unique(vargs[[1]])
    first_indexes <- match(unique_values, vargs[[1]])
    all_indexes <- match(vargs[[1]], unique_values)
    vargs2 <- list()
    vargs2[[names]] <- vargs[[names]][first_indexes]
    res <- do.call("mapply", c(FUN = FUN, vargs2,
                               MoreArgs = list(args[!dovec]),
                               SIMPLIFY = SIMPLIFY, USE.NAMES = USE.NAMES))
    res <- res[all_indexes]
    if(UNLIST) {
      attr <- attributes(res[[1]])
      res <- unlist(res)
      attributes(res) <- attr
    }
    names(res) <- names(vargs[[1]])
    res
  }
  formals(FUNV) <- formals(FUN)
  FUNV
}
