
# get character argument names out of ... with bquote .() evaluaiton in unpack_environment
capture_and_validate_assignment_targets <- function(unpack_environment, ...) {
  force(unpack_environment)
  # capture the arguments unevaluted, and run through bquote
  args <- as.list(do.call(bquote, list(substitute(list(...)), where = unpack_environment)))[-1]
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
    }
    str_args[[i]] <- cargi
  }
  non_nas <- str_args[!is.na(str_args)]
  if(length(non_nas) != length(unique(non_nas))) {
    stop("wrapr::unpack expected all non-NA targets must be unique")
  }
  return(str_args)
}

#' write values into environment
#'
#' @param unpack_environment environment to write into
#' @param str_args array of string-names to write to
#' @param value values to write
#'
#' @keywords internal
#' @export
#'
write_values_into_env <- function(unpack_environment, str_args, value) {
  force(unpack_environment)
  nargs <- length(str_args)
  nvalue <- length(value)
  if(nargs != nvalue) {
    stop(paste0("wrapr::unpack number of returned values is ", nvalue, ", but expecting ", nargs, " values."))
  }
  # up-cycling NULL values case
  if(is.null(value)) {
    value <- vector(mode = 'list', length = nargs)  # list of NULLs
  }
  # write values
  for(i in 1:nargs) {
    argi <- str_args[[i]]
    if(!is.na(argi)) {
      assign(x = argi, value = value[[i]], envir = unpack_environment)
    }
  }
}



#' create an argname carrier that is a function
#' @param ... names to capture for assignments
#' @return an unpacking function
#'
#' @keywords internal
#' @export
#'
unpacker_target <- function(...) {
  # get environment to work in
  unpack_environment <- parent.frame(n = 1)
  str_args <- capture_and_validate_assignment_targets(unpack_environment, ...)
  f <- function(value) {
    unpack_environment <- parent.frame(n = 1)
    force(value)
    # need ::, as re-writing function environment
    wrapr::write_values_into_env(unpack_environment = unpack_environment, str_args = str_args, value = value)
  }
  environment(f) <- new.env(parent = globalenv())
  assign('str_args', str_args, envir = environment(f))
  class(f) <- 'unpacker_target'
  return(f)
}


#' @export
format.unpacker_target <- function(x, ...) {
  str_args <- get('str_args', envir = environment(x))
  return(paste0("unpacker_target(", paste(str_args, collapse = ', '), ")"))
}


#' @export
as.character.unpacker_target <- function(x, ...) {
  format(x, ...)
}


#' @export
print.unpacker_target <- function(x, ...) {
  cat(format(x, ...))
}



# create the unpack object
define_unpacker <- function(object_name) {
  force(object_name)
  f <- function(...) {
    # need ::, as re-writing function environment
    return(wrapr::unpacker_target(...))
  }
  environment(f) <- new.env(parent = globalenv())
  assign('object_name', object_name, envir = environment(f))
  class(f) <- 'unpacker'
  return(f)
}


#' @export
format.unpacker <- function(x, ...) {
  str_args <- get('object_name', envir = environment(x))
  return(str_args)
}


#' @export
as.character.unpacker <- function(x, ...) {
  format(x, ...)
}


#' @export
print.unpacker <- function(x, ...) {
  cat(format(x, ...))
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
  force(self)
  # get environment to work in
  unpack_environment <- parent.frame(n = 1)
  # the array update is going to write an object into the
  # destination environment after returning from this method,
  # try to ensure it is obvious it is the exact
  # object that was already there.
  object_name <- get('object_name', envir = environment(self))
  old_value <- base::mget(object_name,
                          envir = unpack_environment,
                          mode = "any",
                          ifnotfound = list(self),
                          inherits = FALSE)[[1]]
  if(!("unpacker" %in% class(old_value))) {
    stop(paste0("running upacker would overwrite ", object_name, " which is not an unpacker"))
  }
  str_args <- capture_and_validate_assignment_targets(unpack_environment, ...)
  for(si in str_args) {
    if(!is.na(si)) {
      if(si == object_name) {
        stop(paste0("wrapr::unpack, target collided with unpacker name: ", object_name))
      }
    }
  }
  force(value)
  write_values_into_env(unpack_environment = unpack_environment, str_args = str_args, value = value)
  # the return value gets written into executing environment after return
  # R expects this to be self, so do that instead of returning old_value
  return(self)
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
#' @param ... argument names to write to
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
#' @param ... argument names to write to
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


