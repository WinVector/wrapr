
#' Memoizating wrapr to base::Vectorize()
#'
#' Build a wrapped function that applies to each unique argument in a vector of arguments once.
#'
#' Only sensible for pure side-effect free deterministic functions.
#'
#' @seealso \code{\link[base]{Vectorize}}
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
  FNAM <- sQuote("FUN")
  FUNV <- function() {
    args <- lapply(as.list(match.call())[-1L], eval, parent.frame())
    if(length(args)!=1) {
      stop(FNAM, "VectorizeM function: saw more than one argument group")
    }
    names <- if (is.null(names(args)))
      character(length(args))
    else names(args)
    dovec <- names %in% vectorize.args
    vargs <- args[dovec]
    firsts <- match(unique(vargs[[1]]), vargs[[1]])
    vargs2 <- list()
    vargs2[[names]] <- vargs[[names]][firsts]
    res <- do.call("mapply", c(FUN = FUN, vargs2, MoreArgs = list(args[!dovec]),
                               SIMPLIFY = SIMPLIFY, USE.NAMES = USE.NAMES))
    rnames <- names(res)
    names(res) <- as.character(vargs2[[names]])
    res <- res[as.character(vargs[[names]])]
    if(is.null(rnames))  {
      names(res) <- NULL
    } else {
      names(rnames) <- as.character(vargs2[[names]])
      names(res) <- rnames[as.character(vargs[[names]])]
    }
    if(UNLIST) {
      attr <- attributes(res[[1]])
      res <- unlist(res)
      attributes(res) <- attr
    }
    res
  }
  formals(FUNV) <- formals(FUN)
  FUNV
}