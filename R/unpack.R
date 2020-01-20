
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
  named_case <- length(names(args)) > 0
  if(named_case) {
    # named targets case
    if(length(names(args)) != nargs) {
      stop("if using names, all arguments must be named")
    }
    target_names = names(args)
  } else {
    # value targets case
    target_names = args
  }
  str_targets <- character(nargs)
  for(i in 1:nargs) {
    target_i <- target_names[[i]]
    char_target_i <- NA_character_
    if(is.null(target_i)) {
      stop("wrapr::unpack expected all targets to not be NULL")
    }
    if(!(is.name(target_i) || is.character(target_i) || is.na(target_i))) {
      stop("wrapr::unpack expected all targets to be character, name, or NA")
    }
    char_target_i <- as.character(target_i)
    if(length(char_target_i) != 1) {
      stop("wrapr::unpack expected all targets to be length 1.")
    }
    if(!is.na(char_target_i)) {
      if(nchar(char_target_i) < 1) {
        stop("wrapr::unpack expected all targets to be non-empty strings")
      }
      tcargi <- trimws(char_target_i, which = "both")
      if(tcargi != char_target_i) {
        stop("wrapr::unpack expected all targets must not start or end with whitespace")
      }
      if(char_target_i == ".") {
        stop("wrapr::unpack expected all targets must not be .")
      }
    }
    str_targets[[i]] <- char_target_i
  }
  non_nas <- str_targets[!is.na(str_targets)]
  if(length(non_nas) != length(unique(non_nas))) {
    stop("wrapr::unpack expected all non-NA targets must be unique")
  }
  if(named_case) {
    na_posns <- is.na(args)
    values <- as.character(args)  # be less picky on sources, convert them
    values[na_posns] <- NA_character_
    for(i in 1:nargs) {
      source_i <- values[[i]]
      if(is.null(source_i)) {
        stop("wrapr::unpack expected all source names to not be NULL")
      }
      if(length(source_i) != 1) {
        stop("wrapr::unpack expected all source names to be length 1.")
      }
      if(!is.na(source_i)) {
        if(nchar(source_i) < 1) {
          stop("wrapr::unpack expected all source_names to be non-empty strings")
        }
      }
    }
    names(values) <- str_targets
    str_targets <- values
  }
  return(str_targets)
}


#' write values into environment
#'
#' @param unpack_environment environment to write into
#' @param str_args array of string-names to write to (either without names, or names as targets)
#' @param value values to write
#'
#' @keywords internal
#' @noRd
#'
write_values_into_env <- function(unpack_environment, str_args, value) {
  force(unpack_environment)
  nargs <- length(str_args)
  named_case <- length(names(str_args)) > 0
  if(named_case) {
    unique_sources <- unique(str_args) # names lost here
    unique_sources <- unique_sources[!is.na(unique_sources)]
    # up-cycling NULL values case
    if(is.null(value)) {
      value <- vector(mode = 'list', length = length(unique_sources))  # list of NULLs
      names(value) <- unique_sources
    }
    for(source_i in str_args) {
      if(!is.na(source_i)) {
        if(!isTRUE(source_i %in% names(value))) {
          stop(paste0("wrapr::unpack all source names must be in value, ", sQuote(source_i), " is missing"))
        }
      }
    }
    # write values
    for(i in 1:nargs) {
      dest_i <- names(str_args)[[i]]
      source_i <- str_args[[i]]
      value_i <- NA
      if(!is.na(source_i)) {
        value_i <-value[[source_i]]
      }
      assign(x = dest_i, value = value_i, envir = unpack_environment)
    }
  } else {
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
      dest_i <- str_args[[i]]
      source_i <- i
      value_i <- value[[source_i]]
      if(!is.na(dest_i)) {
        assign(x = dest_i, value = value_i, envir = unpack_environment)
      }
    }
  }
}


#' Create a value unpacking object.
#'
#' Create a value unplacking object that records it is stored by name \code{object_name}.
#'
#' @param object_name character, name the object is stored as
#' @return an unpacker
#'
#' @keywords internal
#' @export
#'
define_unpacker <- function(object_name) {
  force(object_name)
  if(!is.null(object_name)) {
    if(!is.character(object_name)) {
      stop("wrapr::define_unpacker object_name must be character when not NULL")
    }
    if(length(object_name) != 1) {
      stop("wrapr::define_unpacker object_naem must be length 1 when not NULL")
    }
  }

  f <- function(value, ...) {
    # get environment to work in
    unpack_environment <- parent.frame(n = 1)
    # get the targets
    str_args <- capture_and_validate_assignment_targets(unpack_environment, ...)
    force(value)
    write_values_into_env(unpack_environment = unpack_environment, str_args = str_args, value = value)
  }

  attr(f, 'object_name') <- object_name
  class(f) <- 'unpacker'
  return(f)
}


#' @export
format.unpacker <- function(x, ...) {
  object_name <- attr(x, 'object_name')
  return(object_name)
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
#' Note: when using \code{[]<-} notation, a reference to the unpacker object is written into the unpacking environment as a side-effect
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
#' # named unpacking
#' # looks like assignment: destination = where_from_in_value
#' d <- data.frame(x = 1:2,
#'                 g=c('test', 'train'),
#'                 stringsAsFactors = FALSE)
#' into[train_set = train, test_set = test] <- split(d, d$g)
#' # train_set and test_set now correctly split
#' print(train_set)
#' print(test_set)
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
  # arg name not passed this deep, the followign sees "*tmp*"
  # object_name <- as.character(deparse(substitute(self)))[[1]]
  object_name <- attr(self, 'object_name')
  if(!is.null(object_name)) {
    old_value <- base::mget(object_name,
                            envir = unpack_environment,
                            mode = "any",
                            ifnotfound = list(self),
                            inherits = FALSE)[[1]]
    if(!("unpacker" %in% class(old_value))) {
      stop(paste0("running upacker would overwrite ", object_name, " which is not an unpacker"))
    }
  }
  str_args <- capture_and_validate_assignment_targets(unpack_environment, ...)
  if(!is.null(object_name)) {
    named_case <- length(names(str_args)) > 0
    if(named_case) {
      for(si in names(str_args)) {
        if(si == object_name) {
          stop(paste0("wrapr::unpack, target collided with unpacker name: ", object_name))
        }
      }
    } else {
      for(si in str_args) {
        if(!is.na(si)) {
          if(si == object_name) {
            stop(paste0("wrapr::unpack, target collided with unpacker name: ", object_name))
          }
        }
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
#' @param value list of values to copy
#' @param ... argument names to write to
#'
#' @examples
#'
#' # named unpacking
#' # looks like assignment: destination = where_from_in_value
#' d <- data.frame(x = 1:2,
#'                 g=c('test', 'train'),
#'                 stringsAsFactors = FALSE)
#' into[train_set = train, test_set = test] <- split(d, d$g)
#' # train_set and test_set now correctly split
#' print(train_set)
#' print(test_set)
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
#' # pipe version
#' list(55, 15) %.>% into(., a, b)
#' print(a)  # now 55
#' print(b)  # now 15
#' # Note: the above example will not work with magrittr pipe,
#' # as in that case the values get written to an intermediate
#' # environment and lost.
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
#' Note: when using \code{[]<-} notation, a reference to the unpacker object is written into the unpacking environment as a side-effect
#' of the implied array assignment.  Also, can not unpack into a variable name same as the un-packer's
#' original declared name (to).
#'
#' @param value list of values to copy
#' @param ... argument names to write to
#'
#' @examples
#'
#' # named unpacking
#' # looks like assignment: destination = where_from_in_value
#' d <- data.frame(x = 1:2,
#'                 g=c('test', 'train'),
#'                 stringsAsFactors = FALSE)
#' to[train_set = train, test_set = test] <- split(d, d$g)
#' # train_set and test_set now correctly split
#' print(train_set)
#' print(test_set)
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
#' # pipe version
#' list(55, 15) %.>% to(., a, b)
#' print(a)  # now 55
#' print(b)  # now 15
#' # Note: the above example will not work with magrittr pipe,
#' # as in that case the values get written to an intermediate
#' # environment and lost.
#'
#' @export
#'
to <- define_unpacker("to")

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
#' original declared name (unpack).
#'
#' @param value list of values to copy
#' @param ... argument names to write to
#'
#' @examples
#'
#' # named unpacking
#' # looks like assignment: destination = where_from_in_value
#' d <- data.frame(x = 1:2,
#'                 g=c('test', 'train'),
#'                 stringsAsFactors = FALSE)
#' unpack[train_set = train, test_set = test] <- split(d, d$g)
#' # train_set and test_set now correctly split
#' print(train_set)
#' print(test_set)
#'
#' # name capture version
#' unpack[a, b] <- list(5, 10)
#' print(a)  # now 5
#' print(b)  # now 10
#'
#' # bquote re-direct to value in variable using .()
#' # Note: the bquote .() step is potentially confusing, as the user
#' # can't immediately see where the value is being assigned to.
#' # Also, quotes are allowed.
#' a <- 'x'
#' unpack[.(a), 'b'] <- list(20, 40)
#' print(x)  # now 20
#' print(b)  # now 40
#' print(a)  # still 'x'
#'
#' # pipe version
#' list(55, 15) %.>% unpack(., a, b)
#' print(a)  # now 55
#' print(b)  # now 15
#' # Note: the above example will not work with magrittr pipe,
#' # as in that case the values get written to an intermediate
#' # environment and lost.
#'
#' @export
#'
unpack <- define_unpacker("unpack")

