
validate_positional_targets <- function(target_names) {
  # extract and validate arguments as names of unpack targets
  nargs <- length(target_names)
  if(nargs < 1) {
    stop("no indices to wrapr::unpacker")
  }
  if(length(names(target_names)) > 0) {
    stop("unexpected names")
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
  if(length(non_nas) <= 0) {
    stop("wrapr::unpack expected some non-NA targets")
  }
  if(length(non_nas) != length(unique(non_nas))) {
    stop("wrapr::unpack expected all non-NA targets must be unique")
  }
  return(str_targets)
}

validate_source_names <- function(source_names) {
  # extract and validate arguments as names of unpack sources
  nargs <- length(source_names)
  if(nargs < 1) {
    stop("no indices to wrapr::unpacker")
  }
  if(length(names(source_names)) > 0) {
    stop("unexpected names")
  }
  str_names <- character(nargs)
  for(i in 1:nargs) {
    source_i <- source_names[[i]]
    char_source_i <- NA_character_
    if(is.null(source_i)) {
      stop("wrapr::unpack expected all sources to not be NULL")
    }
    if(!(is.name(source_i) || is.character(source_i) || is.na(source_i))) {
      stop("wrapr::unpack expected all sources to be character, name, or NA")
    }
    char_source_i <- as.character(source_i)
    if(length(char_source_i) != 1) {
      stop("wrapr::unpack expected all sources to be length 1.")
    }
    if(is.na(char_source_i)) {
      stop("wrapr::unpack expected all sources to not be NA.")
    }
    if(nchar(char_source_i) < 1) {
      stop("wrapr::unpack expected all sources to be non-empty strings")
    }
    tcargi <- trimws(char_source_i, which = "both")
    if(tcargi != char_source_i) {
      stop("wrapr::unpack expected all sources must not start or end with whitespace")
    }
    if(char_source_i == ".") {
      stop("wrapr::unpack expected all sources must not be .")
    }
    str_names[[i]] <- char_source_i
  }
  return(str_names)
}

# validate source and target naming
validate_assignment_named_map <- function(args) {
  if(length(args) < 1) {
    stop("no mapping indices to wrapr::unpacker")
  }
  target_names <- names(args)
  source_names <- args
  names(source_names) <- NULL
  source_names <- validate_source_names(source_names)
  if(length(target_names) <= 0) {
    target_names <- source_names
  } else {
    if(length(target_names) != length(source_names)) {
      stop("expect same number of targets as sources")   # should be unreachable, establish a local invarient
    }
    for(i in 1:length(source_names)) {
      if( is.null(target_names[[i]]) || (!is.character(target_names[i])) || is.na(target_names[[i]])  || (length(target_names[[i]]) != 1) || (nchar(target_names[[i]]) <= 0) ) {
        target_names[[i]] <- source_names[[i]]
      }
    }
  }
  target_names <- validate_positional_targets(target_names)
  if(any(is.na(target_names))) {  # not checked for positional targets
    stop("all target names must not be NA")
  }
  names(source_names) <- target_names
  return(source_names)
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
#' Array-assign form can not use the names: \code{.}, \code{wrapr_private_self}, \code{value}, or the name stored in \code{object_name}.
#' function form can not use the names: \code{.} or \code{wrapr_private_value}. Array-form will wrong own name into working environment
#' as a side-effect.
#'
#' @param object_name character, name the object is stored as
#' @param unnamed_case logical, if TRUE use positional- not name based matching
#' @return an unpacker
#'
#' @keywords internal
#' @export
#'
Unpacker <- function(object_name = NULL, unnamed_case = FALSE) {
  force(object_name)
  force(unnamed_case)
  if(!is.null(object_name)) {
    if(!is.character(object_name)) {
      stop("wrapr::Unpacker object_name must be character when not NULL")
    }
    if(length(object_name) != 1) {
      stop("wrapr::Unpacker object_naem must be length 1 when not NULL")
    }
  }

  f <- function(wrapr_private_value, ...) {
    # get environment to work in
    unpack_environment <- parent.frame(n = 1)
    # get the targets
    # capture the arguments unevaluted, and run through bquote
    str_args <- as.list(do.call(bquote, list(substitute(list(...)), where = unpack_environment)))[-1]
    if(unnamed_case) {
      str_args <- validate_positional_targets(str_args)
    } else {
      str_args <- validate_assignment_named_map(str_args)
    }
    force(wrapr_private_value)
    write_values_into_env(unpack_environment = unpack_environment, str_args = str_args, value = wrapr_private_value)
    invisible(wrapr_private_value)
  }

  attr(f, 'object_name') <- object_name
  attr(f, 'unnamed_case') <- unnamed_case
  class(f) <- "Unpacker"
  return(f)
}


#' @export
format.Unpacker <- function(x, ...) {
  object_name <- attr(x, 'object_name')
  unnamed_case <- attr(x, 'unnamed_case')
  q_name <- "NULL"
  if(!is.null(object_name)) {
    q_name <- sQuote(object_name)
  }
  return(paste0("wrapr::Unpacker(object_name = ", q_name, ", unnamed_case = ", unnamed_case, ")"))
}


#' @export
as.character.Unpacker <- function(x, ...) {
  format(x, ...)
}


#' @export
print.Unpacker <- function(x, ...) {
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
#' of the implied array assignment.
#' Array-assign form can not use the names: \code{.}, \code{wrapr_private_self}, \code{value}, or the name of the unpacker itself.
#'
#' @param wrapr_private_self object implementing the feature, wrapr::unpack
#' @param ... names of to unpack to (can be escaped with bquote \code{.()} notation).
#' @param value list to unpack into values, must have a number of entries equal to number of \code{...} arguments
#' @return wrapr_private_self
#'
#' @examples
#'
#' # named unpacking
#' # looks like assignment: DESTINATION = NAME_VALUE_USING
#' d <- data.frame(x = 1:2,
#'                 g=c('test', 'train'),
#'                 stringsAsFactors = FALSE)
#' to[train_set = train, test_set = test] <- split(d, d$g)
#' # train_set and test_set now correctly split
#' print(train_set)
#' print(test_set)
#' rm(list = c('train_set', 'test_set'))
#'
#' # named unpacking NEWNAME = OLDNAME implicit form
#' # values are matched by name, not index
#' to[train, test] <- split(d, d$g)
#' print(train)
#' print(test)
#' rm(list = c('train', 'test'))
#'
#'
#' @export
#'
`[<-.Unpacker` <- function(wrapr_private_self, ..., value) {
  force(wrapr_private_self)
  # get environment to work in
  unpack_environment <- parent.frame(n = 1)
  # capture .. args
  str_args <- as.list(do.call(bquote, list(substitute(list(...)), where = unpack_environment)))[-1]
  # the array update is going to write an object into the
  # destination environment after returning from this method,
  # try to ensure it is obvious it is the exact
  # object that was already there.
  # arg name not passed this deep, the followign sees "*tmp*"
  # object_name <- as.character(deparse(substitute(wrapr_private_self)))[[1]]
  object_name <- attr(wrapr_private_self, 'object_name')
  if(!is.null(object_name)) {
    old_value <- base::mget(object_name,
                            envir = unpack_environment,
                            mode = "any",
                            ifnotfound = list(NULL),
                            inherits = FALSE)[[1]]
    if(!is.null(old_value)) {
      if(!("Unpacker" %in% class(old_value))) {
        stop(paste0("running upacker would overwrite ", object_name, " which is not an unpacker"))
      }
    }
    old_value <- NULL
  }
  unnamed_case <- isTRUE(attr(wrapr_private_self, 'unnamed_case'))
  if(unnamed_case) {
    str_args <- validate_positional_targets(str_args)
  } else {
    str_args <- validate_assignment_named_map(str_args)
  }
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
  # R expects this to be wrapr_private_self, so do that instead of returning old_value
  return(wrapr_private_self)
}


#' Unpack or bind values by names into the calling environment.
#'
#' Unpacks or binds values into the calling environment. Uses \code{bquote} escaping.
#' NULL is a special case that is unpacked to all targets. NA targets are skipped.
#' All non-NA target names must be unique.
#'
#' Similar to \code{Python} tuple unpacking, \code{zeallot}'s arrow, and to \code{vadr::bind}.
#'
#' Note: a reference to the unpacker object is written into the unpacking environment as a side-effect
#' of the implied array assignment.
#' Array-assign form can not use the names: \code{.}, \code{wrapr_private_self}, \code{value}, or \code{into}.
#' Function form can not use the names: \code{.} or \code{wrapr_private_value}.
#'
#' @param wrapr_private_value list of values to copy
#' @param ... argument names to write to
#'
#' @examples
#'
#' # named unpacking
#' # looks like assignment: DESTINATION = NAME_VALUE_USING
#' d <- data.frame(x = 1:2,
#'                 g=c('test', 'train'),
#'                 stringsAsFactors = FALSE)
#' into[train_set = train, test_set = test] <- split(d, d$g)
#' # train_set and test_set now correctly split
#' print(train_set)
#' print(test_set)
#' rm(list = c('train_set', 'test_set'))
#'
#' # named unpacking NEWNAME = OLDNAME implicit form
#' # values are matched by name, not index
#' into[train, test] <- split(d, d$g)
#' print(train)
#' print(test)
#' rm(list = c('train', 'test'))
#'
#' # function version
#' into(split(d, d$g), train, test)
#' print(train)
#' print(test)
#' rm(list = c('train', 'test'))
#'
#' # pipe version
#' split(d, d$g) %.>% into(., train, test)
#' print(train)
#' print(test)
#' rm(list = c('train', 'test'))
#' # Note: above is wrapr dot-pipe, piping does not currently work with
#' # magrittr pipe due to magrittr's introduction of temporary
#' # intermediate environments during evaluation.
#'
#'
#' @export
#'
into <- Unpacker(object_name = "into", unnamed_case = FALSE)

#' Unpack or bind values by names into the calling environment.
#'
#' Unpacks or binds values into the calling environment. Uses \code{bquote} escaping.
#' NULL is a special case that is unpacked to all targets. NA targets are skipped.
#' All non-NA target names must be unique.
#'
#' Similar to \code{Python} tuple unpacking, \code{zeallot}'s arrow, and to \code{vadr::bind}.
#'
#' Note: when using \code{[]<-} notation, a reference to the unpacker object is written into the unpacking environment as a side-effect
#' of the implied array assignment.
#' Array-assign form can not use the names: \code{.}, \code{wrapr_private_self}, \code{value}, or \code{to}.
#' function form can not use the names: \code{.} or \code{wrapr_private_value}.
#'
#' @param wrapr_private_value list of values to copy
#' @param ... argument names to write to
#'
#' @examples
#'
#' # named unpacking
#' # looks like assignment: DESTINATION = NAME_VALUE_USING
#' d <- data.frame(x = 1:2,
#'                 g=c('test', 'train'),
#'                 stringsAsFactors = FALSE)
#' to[train_set = train, test_set = test] <- split(d, d$g)
#' # train_set and test_set now correctly split
#' print(train_set)
#' print(test_set)
#' rm(list = c('train_set', 'test_set'))
#'
#' # named unpacking NEWNAME = OLDNAME implicit form
#' # values are matched by name, not index
#' to[train, test] <- split(d, d$g)
#' print(train)
#' print(test)
#' rm(list = c('train', 'test'))
#'
#' # function version
#' to(split(d, d$g), train, test)
#' print(train)
#' print(test)
#' rm(list = c('train', 'test'))
#'
#' # pipe version
#' split(d, d$g) %.>% to(., train, test)
#' print(train)
#' print(test)
#' rm(list = c('train', 'test'))
#' # Note: above is wrapr dot-pipe, piping does not currently work with
#' # magrittr pipe due to magrittr's introduction of temporary
#' # intermediate environments during evaluation.
#'
#' @export
#'
to <- Unpacker(object_name = "to", unnamed_case = FALSE)

#' Unpack or bind values by names into the calling environment.
#'
#' Unpacks or binds values into the calling environment. Uses \code{bquote} escaping.
#' NULL is a special case that is unpacked to all targets. NA targets are skipped.
#' All non-NA target names must be unique.
#'
#' Similar to \code{Python} tuple unpacking, \code{zeallot}'s arrow, and to \code{vadr::bind}.
#'
#' Note: a reference to the unpacker object is written into the unpacking environment as a side-effect
#' of the implied array assignment.
#' Array-assign form can not use the names: \code{.}, \code{wrapr_private_self}, \code{value}, or \code{unpack}.
#' Function form can not use the names: \code{.} or \code{wrapr_private_value}.
#'
#' @param wrapr_private_value list of values to copy
#' @param ... argument names to write to
#'
#' @examples
#'
#' # named unpacking
#' # looks like assignment: DESTINATION = NAME_VALUE_USING
#' d <- data.frame(x = 1:2,
#'                 g=c('test', 'train'),
#'                 stringsAsFactors = FALSE)
#' unpack[train_set = train, test_set = test] <- split(d, d$g)
#' # train_set and test_set now correctly split
#' print(train_set)
#' print(test_set)
#' rm(list = c('train_set', 'test_set'))
#'
#' # named unpacking NEWNAME = OLDNAME implicit form
#' # values are matched by name, not index
#' unpack[train, test] <- split(d, d$g)
#' print(train)
#' print(test)
#' rm(list = c('train', 'test'))
#'
#' # function version
#' unpack(split(d, d$g), train, test)
#' print(train)
#' print(test)
#' rm(list = c('train', 'test'))
#'
#' # pipe version
#' split(d, d$g) %.>% unpack(., train, test)
#' print(train)
#' print(test)
#' rm(list = c('train', 'test'))
#' # Note: above is wrapr dot-pipe, piping does not currently work with
#' # magrittr pipe due to magrittr's introduction of temporary
#' # intermediate environments during evaluation.
#'
#' @export
#'
unpack <- Unpacker(object_name = "unpack", unnamed_case = FALSE)

#' Unpack or bind values by index or names into the calling environment.
#'
#' Unpacks or binds values into the calling environment. Uses \code{bquote} escaping.
#' NULL is a special case that is unpacked to all targets. NA targets are skipped.
#' All non-NA target names must be unique.
#'
#' Similar to \code{Python} tuple unpacking, \code{zeallot}'s arrow, and to \code{vadr::bind}.
#'
#' Note: a reference to the unpacker object is written into the unpacking environment as a side-effect
#' of the implied array assignment.
#' Array-assign form can not use the names: \code{.}, \code{wrapr_private_self}, \code{value}, or \code{unpack}.
#' Function form can not use the names: \code{.} or \code{wrapr_private_value}.
#'
#' @param wrapr_private_value list of values to copy
#' @param ... argument names to write to
#'
#' @examples
#'
#' # un-named unpacking
#' # values matchced by index
#' unpack_i[a, b] <- list(5, 10)
#' print(a)  # now 5
#' print(b)  # now 10
#'
#' # un-named unpacking
#' #' # values matchced by index
#' # bquote re-direct to value in variable using .()
#' # Note: the bquote .() step is potentially confusing, as the user
#' # can't immediately see where the value is being assigned to.
#' # Also, quotes are allowed.
#' a <- 'x'
#' unpack_i[.(a), 'b'] <- list(20, 40)
#' print(x)  # now 20
#' print(b)  # now 40
#' print(a)  # still 'x'
#'
#' # un-named unpacking
#' #' # values matchced by index
#' # piped example
#' list(55, 15) %.>% unpack_i(., a, b)
#' print(a)  # now 55
#' print(b)  # now 15
#' # Note: the above example will not work with magrittr pipe,
#' # as in that case the values get written to an intermediate
#' # environment and lost.
#'
#' @export
#'
unpack_i <- Unpacker(object_name = "unpack", unnamed_case = TRUE)

