
#' Unpack or bind values into the calling environment.
#'
#' Unpacks or binds values into the calling environment. Uses \code{bquote} escaping.
#' NULL is a special case that is unpacked to all targets. NA targets are skipped.
#' All non-NA target names must be unique.
#'
#' Similar to \code{Python} tuple unpacking, \code{zeallot}'s arrow, and to \code{vadr::bind}.
#'
#' Note: a reference to the unpacker object is written into the unpacking environment as a side-effect
#' of the implied array assignment.  Also, can not unpack into a variable name same as the un-packer's
#' original declared name (to/into).
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
  # get environment to work in
  unpack_environment <- parent.frame(n = 1)
  # capture the arguments unevaluted, and run through bquote
  args <- as.list(do.call(bquote, list(substitute(list(...)), where = unpack_environment)))[-1]
  # the array update is going to write an object into the
  # destination environment after returning from this method,
  # try to ensure it is obvious it is the exact
  # object that was already there.
  old_value <- base::mget(self$object_name,
                          envir = unpack_environment,
                          mode = "any",
                          ifnotfound = list(self),
                          inherits = FALSE)[[1]]
  if(!("unpacker" %in% class(old_value))) {
    stop(paste0("running upackers would overwrite ", self$object_name, " which is not an unpacker"))
  }
  # extract and validate arguments as names of unpack targets
  nargs <- length(args)
  if(nargs < 1) {
    stop("no indices to wrapr::unpacker")
  }
  str_args <- character(nargs)
  for(i in 1:nargs) {
    argi <- args[[i]]
    cargi <- NA_character_
    if(is.null(argi)) {
      stop("wrapr::unpack expected all targets to not be NULL")
    }
    if(!(is.name(argi) || is.character(argi) || is.na(argi))) {
      stop("wrapr::unpack expected all targets to be character, name, or NA")
    }
    cargi <- as.character(argi)
    if(length(cargi) != 1) {
      stop("wrapr::unpack expected all targets to be length 1.")
    }
    if(!is.na(cargi)) {
      if(nchar(cargi) < 1) {
        stop("wrapr::unpack expected all targets to be non-empty strings")
      }
      if(cargi == self$object_name) {
        stop(paste0("wrapr::unpack, target collided with unpacker name: ", self$object_name))
      }
    }
    str_args[[i]] <- cargi
  }
  non_nas <- str_args[!is.na(str_args)]
  if(length(non_nas) != length(unique(non_nas))) {
    stop("wrapr::unpack expected all non-NA targets must be unique")
  }
  # up-cycling NULL values case
  if(is.null(value)) {
    value <- vector(mode = 'list', length = nargs)  # list of NULLs
  }
  nvalue <- length(value)
  if(nargs != nvalue) {
    stop(paste0("wrapr::unpack number of returned values is ", nvalue, ", but expecting ", nargs, " values."))
  }
  # write values
  for(i in 1:nargs) {
    argi <- str_args[[i]]
    if(!is.na(argi)) {
      assign(x = argi, value = value[[i]], envir = unpack_environment)
    }
  }
  # the return value gets written into executing environment after return
  return(old_value)
}

define_unpacker <- function(object_name) {
  r <- list(object_name = object_name)
  class(r) <- 'unpacker'
  return(r)
}

#' Unpack or bind values into the calling environment.
#'
#' Unpacks or binds values into the calling environment. Uses \code{bquote} escaping.
#' NULL is a special case that is unpacked to all targets. NA targets are skipped.
#' All non-NA target names must be unique.
#'
#' Similar to \code{Python} tuple unpacking, \code{zeallot}'s arrow, and to \code{vadr::bind}.
#'
#' Note: a reference to the unpacker object is written into the unpacking environment as a side-effect
#' of the implied array assignment.  Also, can not unpack into a variable name same as the un-packer's
#' original declared name (into).
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
into <- define_unpacker("into")

#' Unpack or bind values into the calling environment.
#'
#' Unpacks or binds values into the calling environment. Uses \code{bquote} escaping.
#' NULL is a special case that is unpacked to all targets. NA targets are skipped.
#' All non-NA target names must be unique.
#'
#' Similar to \code{Python} tuple unpacking, \code{zeallot}'s arrow, and to \code{vadr::bind}.
#'
#' Note: a reference to the unpacker object is written into the unpacking environment as a side-effect
#' of the implied array assignment.  Also, can not unpack into a variable name same as the un-packer's
#' original declared name (to).
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
to <- define_unpacker("to")

