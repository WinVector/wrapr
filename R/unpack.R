
#' Unpack or bind values into the calling environment.
#'
#' Unpacks or binds values into the calling environment. Uses \code{bquote} escaping.
#' NULL is a special case that is unpacked to all targets.
#'
#' Similar to \code{Python} tuple unpacking, \code{zeallot}'s arrow, and to \code{vadr::bind}.
#'
#' @param self object implementing the feature, wrapr::unpack
#' @param ... names of to unpack to (can be escaped with bquote \code{.()} notation).
#' @param value list to unpack into values, must have a number of entries equal to number of \code{...} arguments
#' @return self
#'
#' @examples
#'
#' # name capture version
#' into[a, b] <- list(5, 10)
#' print(a)  # now 5
#' print(b)  # now 10
#'
#' # bquote re-direct to value in variable using .()
#' # Note: the bquote .() step is potentially confusing, as the user
#' # can't immediately see where the value is being assigned to.
#' # Also, quotes are allowed.
#' a <- 'x'
#' into[.(a), 'b'] <- list(20, 40)
#' print(x)  # now 20
#' print(b)  # now 40
#' print(a)  # still 'x'
#'
#' @export
#'
`[<-.unpacker` <- function(self, ..., value) {
  unpack_environment <- parent.frame(n = 1)
  # capture the arguments unevaluted, and run through bquote
  args <- as.list(do.call(bquote, list(substitute(list(...)), where = unpack_environment)))[-1]
  nargs <- length(args)
  if(nargs < 1) {
    stop("no indices to wrapr::unpacker")
  }
  str_args <- character(nargs)
  for(i in 1:nargs) {
    argi <- args[[i]]
    if(is.null(argi)) {
      stop("wrapr::unpack expected all targets to not be NULL")
    }
    if(!(is.name(argi) || is.character(argi))) {
      stop("wrapr::unpack expected all targets to be names or strings.")
    }
    if(length(argi) != 1) {
      stop("wrapr::unpack expected all targets to be length 1.")
    }
    cargi <- as.character(argi)
    if(is.na(cargi)) {
      stop("wrapr::unpack expected all targets to be not be NA.")
    }
    if(nchar(cargi) < 1) {
      stop("wrapr::unpack expected all targets to be non-empty strings")
    }
    str_args[[i]] <- cargi
  }
  # up-cycling NULL case
  if(is.null(value)) {
    value <- vector(mode = 'list', length=nargs)  # list of NULLs
  }
  nvalue <- length(value)
  if(nargs != nvalue) {
    stop(paste0("wrapr::unpack number of returned values is ", nvalue, ", but expecting ", nargs, " values."))
  }
  for(i in 1:nargs) {
    argi <- str_args[[i]]
    assign(x = argi, value = value[[i]], envir = unpack_environment)
  }
  return(self)
}

define_unpacker <- function() {
  r <- list()
  class(r) <- 'unpacker'
  return(r)
}

#' Unpack or bind values into the calling environment.
#'
#' Unpacks or binds values into the calling environment. Uses bquote escaping.
#' NULL is a special case that is unpacked to all targets.
#'
#' Similar to \code{Python} tuple unpacking, \code{zeallot}'s arrow, and to \code{vadr::bind}.
#'
#' @examples
#'
#' # name capture version
#' into[a, b] <- list(5, 10)
#' print(a)  # now 5
#' print(b)  # now 10
#'
#' # bquote re-direct to value in variable using .()
#' # Note: the bquote .() step is potentially confusing, as the user
#' # can't immediately see where the value is being assigned to.
#' # Also, quotes are allowed.
#' a <- 'x'
#' into[.(a), 'b'] <- list(20, 40)
#' print(x)  # now 20
#' print(b)  # now 40
#' print(a)  # still 'x'
#'
#' @export
#'
into <- define_unpacker()

#' Unpack or bind values into the calling environment.
#'
#' Unpacks or binds values into the calling environment. Uses bquote escaping.
#' NULL is a special case that is unpacked to all targets.
#'
#' Similar to \code{Python} tuple unpacking, \code{zeallot}'s arrow, and to \code{vadr::bind}.
#'
#' @examples
#'
#' # name capture version
#' to[a, b] <- list(5, 10)
#' print(a)  # now 5
#' print(b)  # now 10
#'
#' # bquote re-direct to value in variable using .()
#' # Note: the bquote .() step is potentially confusing, as the user
#' # can't immediately see where the value is being assigned to.
#' # Also, quotes are allowed.
#' a <- 'x'
#' to[.(a), 'b'] <- list(20, 40)
#' print(x)  # now 20
#' print(b)  # now 40
#' print(a)  # still 'x'
#'
#' @export
#'
to <- into

