

#' Re-write captured \code{...} arguments as assignments.
#'
#' Re-write captured \code{...} arguments as a \code{c(DESTINATION = TARGET)} character vector.
#' Suggested capture code is: \code{substitute(list(...))}. Allows \code{bquote} \code{.()} substitution.
#'
#' @param captured_dots captured \code{...}.
#' @param unpack_environment environment to look in
#' @param allow_dot_on_left logical if TRUE allow forms like \code{.(a) = a} and \code{.(a)}.
#' @returns named character vector describing the desired mapping.
#'
#' @examples
#'
#' f <- function(...) {
#'   unpack_environment <- parent.frame(n = 1)
#'   orig_args <- substitute(list(...))
#'   grab_assignments_from_dots(orig_args, unpack_environment)
#' }
#' f(a, c = d, e := f, g <- h, i -> j)
#' # should equal c('a', 'c' = 'd', 'e' = 'f', 'g' = 'h', 'j' = 'i')
#'
#' @keywords internal
#'
#' @export
#'
grab_assignments_from_dots <- function(captured_args, unpack_environment = parent.frame(), allow_dot_on_left = FALSE) {
  force(unpack_environment)
  if(!allow_dot_on_left) {
    nms <- names(captured_args)
    for(i in seqi(2, length(captured_args))) {
      ai <- captured_args[[i]]
      if(missing(ai)) {
        stop("unexpected missing argument, this is often the symptom of an extra comma in your argument list")
      }
      # .(a) := a case
      if(is.call(ai) && (as.character(ai[[1]])[[1]] == ':=')) {
        if((length(ai) >= 2) && is.call(ai[[2]]) && (as.character(ai[[2]])[[1]] == '.')) {
          stop("bquote .() notation not allowed on left side of expressions")
        }
      }
      # .(a) case
      if(is.call(ai) && (as.character(ai[[1]])[[1]] == '.')) {
        if((i > length(nms)) || (nchar(nms[[i]]) == 0)) {
          stop("bquote .() notation allowed as whole expression")
        }
      }
    }
  }
  captured_dots <- as.list(do.call(bquote,
                                   list(captured_args,
                                        where = unpack_environment),
                                   envir = unpack_environment))[-1]
  nargs <- length(captured_dots)
  if(nargs <= 0) {
    return(character(0))
  }
  names <- names(captured_dots)
  if(length(names) <= 0) {
    names <- character(nargs)
    names[seq_len(nargs)] <- ""
  }
  values <- character(nargs)
  for(i in seq_len(nargs)) {
    ni <- names[[i]]
    vi <- captured_dots[[i]]
    if(is.call(vi)) {
      if(nchar(ni) > 0) {
        stop("wrapr::grab_assignments_from_dots call-position can not also have a name")
      }
      if(length(vi) != 3) {
        stop("wrapr::grab_assignments_from_dots call-position must be length 3")
      }
      if(!(as.character(vi[[1]]) %in% c('=', '<-', '->', ':=', '%:=%'))) {
        stop("wrapr::grab_assignments_from_dots call must be one of '=', '<-', '->', ':=', or '%:=%'.")
      }
      ni <- vi[[2]]
      vi <- vi[[3]]
    }
    if(length(ni) != 1) {
      stop("wrapr::grab_assignments_from_dots, names must be length 1")
    }
    if(is.name(ni)) {
      ni <- as.character(ni)[[1]]
    }
    if(is.na(ni)) {
      stop("wrapr::grab_assignments_from_dots, names must not be NA")
    }
    if(!is.character(ni)) {
      stop("wrapr::grab_assignments_from_dots, names must names or character")
    }
    if(length(vi) != 1) {
      stop("wrapr::grab_assignments_from_dots, values must be length 1")
    }
    if(is.name(vi)) {
      vi <- as.character(vi)[[1]]
    }
    if(is.na(vi)) {
      vi <- NA_character_
    }
    if(!is.character(vi)) {
      stop("wrapr::grab_assignments_from_dots, values must names or character")
    }
    if(nchar(vi) < 1) {
      stop("wrapr::grab_assignments_from_dots, values must not be empty (can happen with if an extra comma is present in call)")
    }
    names[[i]] <- ni
    values[[i]] <- vi
  }
  if(all(nchar(names) <= 0)) {
    names <- NULL
  } else {
    non_empty_names <- names[nchar(names)>0]
    if(length(unique(non_empty_names)) != length(non_empty_names)) {
      stop("wrapr::grab_assignments_from_dots, target names must be unique")
    }
  }
  names(values) <- names
  return(values)
}


validate_positional_targets <- function(target_names, extra_forbidden_names = NULL) {
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
      if(char_target_i %in% extra_forbidden_names) {
        stop(paste0("wrapr::unpack expected all targets must not be ", char_target_i))
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
validate_assignment_named_map <- function(args, extra_forbidden_names = NULL) {
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
  target_names <- validate_positional_targets(target_names, extra_forbidden_names = extra_forbidden_names)
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
    missing_items <- c()
    for(source_i in str_args) {
      if(!is.na(source_i)) {
        if(!isTRUE(source_i %in% names(value))) {
          missing_items <- c(missing_items, source_i)
        }
      }
    }
    if(length(missing_items) > 0) {
      stop(paste0("wrapr::unpack all source names must be in value, missing: ",
                  paste(sQuote(missing_items), collapse = ', '),
                  '.'))
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


unpack_impl <- function(..., unpack_environment, value, str_args, object_name = NULL, our_class = "Unpacker") {
  wrapr::stop_if_dot_args(substitute(list(...)), "wrapr:::unpack_impl")
  force(unpack_environment)
  if(!is.null(object_name)) {
    old_value <- base::mget(object_name,
                            envir = unpack_environment,
                            mode = "any",
                            ifnotfound = list(NULL),
                            inherits = FALSE)[[1]]
    if(!is.null(old_value)) {
      if(!(our_class %in% class(old_value))) {
        stop(paste0("wrapr:::unpack_impl running upacker would overwrite ", object_name, " which is not an Unpacker"))
      }
    }
    old_value <- NULL
  }
  str_args <- validate_assignment_named_map(str_args, extra_forbidden_names = object_name)
  force(value)
  write_values_into_env(unpack_environment = unpack_environment, str_args = str_args, value = value)
}


mk_unpack_single_arg_fn <- function(str_args, object_name, our_class, unpack_environment) {
  force(str_args)
  force(object_name)
  force(our_class)
  force(unpack_environment)
  str_args <- validate_assignment_named_map(str_args, extra_forbidden_names = object_name)

  # build function return in a fairly clean environment
  f <- function(.) {
    unpack_impl(unpack_environment = unpack_environment,
                value = .,
                str_args = str_args,
                object_name = NULL,  # this path doesn't write self
                our_class = our_class)
    invisible(.)
  }

  attr(f, 'object_name') <- object_name
  attr(f, 'str_args') <- str_args
  attr(f, 'unpack_environment') <- unpack_environment
  class(f) <- our_class
  return(f)
}


#' Create a value unpacking object (function version).
#'
#' Create a value unplacking object that records it is stored by name \code{object_name} (function version).
#'
#' Array-assign form can not use the names: \code{.}, \code{wrapr_private_self}, \code{value}, or the name stored in \code{object_name}.
#' function form can not use the names: \code{.} or \code{wrapr_private_value}.
#' Array-form with \code{=}, \code{<-}, \code{->} will write own name into working environment
#' as a side-effect. Array-form with \code{:=} does not have the side-effect.
#'
#'
#' @param object_name character, name the object is stored as
#' @return an unpacker
#'
#' @keywords internal
#' @export
#'
UnpackerF <- function(object_name = NULL) {
  force(object_name)
  if(!is.null(object_name)) {
    if(!is.character(object_name)) {
      stop("wrapr::UnpackerF object_name must be character when not NULL")
    }
    if(length(object_name) != 1) {
      stop("wrapr::UnpackerF object_name must be length 1 when not NULL")
    }
  }

  # build function return
  f <- function(wrapr_private_value, ...) {
    # get environment to work in
    unpack_environment <- parent.frame(n = 1)
    # get the targets
    # capture the arguments unevaluted, and run through bquote
    orig_args <- substitute(list(...))
    str_args <- grab_assignments_from_dots(orig_args, unpack_environment)
    unpack_impl(unpack_environment = unpack_environment,
                value = wrapr_private_value,
                str_args = str_args,
                object_name = NULL,   # this path doesn't cause an over-write
                our_class = "Unpacker")
    invisible(wrapr_private_value)
  }

  attr(f, 'object_name') <- object_name
  attr(f, 'dotpipe_eager_eval_bracket') <- TRUE
  attr(f, 'dotpipe_eager_eval_function') <- FALSE
  class(f) <- "Unpacker"
  return(f)
}


#' Create a value unpacking object (eager pipe version).
#'
#' Create a value unplacking object that records it is stored by name \code{object_name} (eager pipe version).
#'
#' Array-assign form can not use the names: \code{.}, \code{wrapr_private_self}, \code{value}, or the name stored in \code{object_name}.
#' function form can not use the names: \code{.} or \code{wrapr_private_value}.
#' Array-form with \code{=}, \code{<-}, \code{->} will write own name into working environment
#' as a side-effect. Array-form with \code{:=} does not have the side-effect.
#'
#'
#' @param object_name character, name the object is stored as
#' @return an unpacker
#'
#' @keywords internal
#' @export
#'
UnpackerP <- function(object_name = NULL) {
  force(object_name)
  if(!is.null(object_name)) {
    if(!is.character(object_name)) {
      stop("wrapr::UnpackerP object_name must be character when not NULL")
    }
    if(length(object_name) != 1) {
      stop("wrapr::UnpackerP object_name must be length 1 when not NULL")
    }
  }

  # build function return
  f <- function(...) {
    # get environment to work in
    unpack_environment <- parent.frame(n = 1)
    # get the targets
    # capture the arguments unevaluted, and run through bquote
    orig_args <- substitute(list(...))
    str_args <- grab_assignments_from_dots(orig_args, unpack_environment)
    bound_f <- mk_unpack_single_arg_fn(str_args = str_args,
                                       object_name = object_name,
                                       our_class = "UnpackTarget",
                                       unpack_environment = unpack_environment)
    return(bound_f)
  }
  attr(f, 'object_name') <- object_name
  attr(f, 'dotpipe_eager_eval_bracket') <- TRUE
  attr(f, 'dotpipe_eager_eval_function') <- TRUE
  class(f) <- "Unpacker"
  return(f)
}


#' @export
format.Unpacker <- function(x, ...) {
  object_name <- attr(x, 'object_name')
  q_name <- "NULL"
  if(!is.null(object_name)) {
    q_name <- sQuote(object_name)
  }
  dotpipe_eager_eval_function <- isTRUE(attr(x, 'dotpipe_eager_eval_function'))
  return(paste0("wrapr::Unpacker(object_name = ", q_name, ", dotpipe_eager_eval_function = ", dotpipe_eager_eval_function ,")",
                "\n # May have been written into your environment as a side-effect of ",
                "\n # ", object_name, '[...]<-',
                "\n # For details, please see:",
                "\n # https://winvector.github.io/wrapr/articles/multi_assign.html"))
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
#' Note: when using \code{[]<-} notation, a reference to the unpacker object is written into the unpacking environment as a side-effect
#' of the implied array assignment. \code{:=} assigment does not have this side-effect.
#' Array-assign form can not use the names: \code{.}, \code{wrapr_private_self}, \code{value}, or the name of the unpacker itself.
#' For more details please see here \url{https://win-vector.com/2020/01/20/unpack-your-values-in-r/}.
#'
#' Related work includes \code{Python} tuple unpacking, \code{zeallot}'s arrow, and \code{vadr::bind}.
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
#' to[train_set = train, test_set = test] := split(d, d$g)
#' # train_set and test_set now correctly split
#' print(train_set)
#' print(test_set)
#' rm(list = c('train_set', 'test_set'))
#'
#' # named unpacking NEWNAME = OLDNAME implicit form
#' # values are matched by name, not index
#' to[train, test] := split(d, d$g)
#' print(train)
#' print(test)
#' rm(list = c('train', 'test'))
#'
#' # bquote example
#' train_col_name <- 'train'
#' test_col_name <- 'test'
#' to[train = .(train_col_name), test = .(test_col_name)] := split(d, d$g)
#' print(train)
#' print(test)
#' rm(list = c('train', 'test'))
#'
#' @export
#'
`[<-.Unpacker` <- function(wrapr_private_self, ..., value) {
  force(wrapr_private_self)
  # get environment to work in
  unpack_environment <- parent.frame(n = 1)
  # capture ... args
  orig_args <- substitute(list(...))
  str_args <- grab_assignments_from_dots(orig_args, unpack_environment)
  # the array update is going to write an object into the
  # destination environment after returning from this method,
  # try to ensure it is obvious it is the exact
  # object that was already there.
  # arg name not passed this deep, the followign sees "*tmp*"
  # object_name <- as.character(deparse(substitute(wrapr_private_self)))[[1]]
  object_name <- attr(wrapr_private_self, 'object_name')
  unpack_impl(unpack_environment = unpack_environment,
              value = value,
              str_args = str_args,
              object_name = object_name,
              our_class = "Unpacker")
  # the return value gets written into executing environment after return
  # R expects this to be wrapr_private_self, so do that instead of returning old_value
  return(wrapr_private_self)
}



#' Prepare for unpack or bind values into the calling environment.
#'
#' Prepare for unpack or bind values into the calling environment.  This makes pipe to behavior very
#' close to assign to behavior for the Unpacker class.
#'
#' @param wrapr_private_self object implementing the feature, wrapr::unpack
#' @param ... names of to unpack to (can be escaped with bquote \code{.()} notation).
#' @return prepared unpacking object
#'
#' @export
#'
`[.Unpacker` <- function(wrapr_private_self, ...) {
  force(wrapr_private_self)
  # get environment to work in
  unpack_environment <- parent.frame(n = 1)
  # capture .. args
  orig_args <- substitute(list(...))
  str_args <- grab_assignments_from_dots(orig_args, unpack_environment)
  object_name <- attr(wrapr_private_self, 'object_name')
  return(mk_unpack_single_arg_fn(str_args = str_args,
                                 object_name = object_name,
                                 our_class = "UnpackTarget",
                                 unpack_environment = unpack_environment))
}


#' @export
format.UnpackTarget <- function(x, ...) {
  object_name <- attr(x, 'object_name')
  str_args <- attr(x, 'str_args')
  unpack_environment <- attr(x, 'unpack_environment')
  q_name <- "NULL"
  if(!is.null(object_name)) {
    q_name <- sQuote(object_name)
  }
  return(paste0("wrapr::UnpackTarget(",
                "\n\tobject_name = ", q_name,
                ",\n\tstr_args = ", map_to_char(str_args),
                ",\n\tunpack_environment = ", format(unpack_environment),
                ")"))
}


#' @export
as.character.UnpackTarget <- function(x, ...) {
  format(x, ...)
}


#' @export
print.UnpackTarget <- function(x, ...) {
  cat(format(x, ...))
}




#' Unpack or bind values by names into the calling environment, eager eval (no-dot) variation.
#'
#' Unpacks or binds values into the calling environment, eager eval (no-dot) variation. Uses \code{bquote} escaping.
#' NULL is a special case that is unpacked to all targets. NA targets are skipped.
#' All non-NA target names must be unique.
#'
#' Note: when using \code{[]<-} notation, a reference to the unpacker object is written into the unpacking environment as a side-effect
#' of the implied array assignment. \code{:=} assigment does not have this side-effect.
#' Array-assign form can not use the names: \code{.}, \code{wrapr_private_self}, \code{value}, or \code{to}.
#' function form can not use the names: \code{.} or \code{wrapr_private_value}.
#' For more detials please see here \url{https://win-vector.com/2020/01/20/unpack-your-values-in-r/}.
#'
#' Related work includes \code{Python} tuple unpacking, \code{zeallot}'s arrow, and \code{vadr::bind}.
#'
#' @param ... argument names to write to
#' @return a UnpackTarget
#'
#' @examples
#'
#' # named unpacking
#' # looks like assignment: DESTINATION = NAME_VALUE_USING
#' d <- data.frame(x = 1:2,
#'                 g=c('test', 'train'),
#'                 stringsAsFactors = FALSE)
#' to[train_set = train, test_set = test] := split(d, d$g)
#' # train_set and test_set now correctly split
#' print(train_set)
#' print(test_set)
#' rm(list = c('train_set', 'test_set'))
#'
#' # named unpacking NEWNAME = OLDNAME implicit form
#' # values are matched by name, not index
#' to[train, test] := split(d, d$g)
#' print(train)
#' print(test)
#' rm(list = c('train', 'test'))
#'
#' # pipe version (notice no dot)
#' split(d, d$g) %.>% to(train, test)
#' print(train)
#' print(test)
#' rm(list = c('train', 'test'))
#' # Note: above is wrapr dot-pipe, piping does not currently work with
#' # magrittr pipe due to magrittr's introduction of temporary
#' # intermediate environments during evaluation.
#'
#' # bquote example
#' train_col_name <- 'train'
#' test_col_name <- 'test'
#' to[train = .(train_col_name), test = .(test_col_name)] := split(d, d$g)
#' print(train)
#' print(test)
#' rm(list = c('train', 'test'))
#'
#' @export
#'
to <- UnpackerP(object_name = "to")

#' Unpack or bind values by names into the calling environment.
#'
#' Unpacks or binds values into the calling environment. Uses \code{bquote} escaping.
#' NULL is a special case that is unpacked to all targets. NA targets are skipped.
#' All non-NA target names must be unique.
#'
#' Note: when using \code{[]<-} notation, a reference to the unpacker object is written into the unpacking environment as a side-effect
#' of the implied array assignment. \code{:=} assigment does not have this side-effect.
#' Array-assign form can not use the names: \code{.}, \code{wrapr_private_self}, \code{value}, or \code{unpack}.
#' Function form can not use the names: \code{.} or \code{wrapr_private_value}.
#' For more details please see here \url{https://win-vector.com/2020/01/20/unpack-your-values-in-r/}.
#'
#' Related work includes \code{Python} tuple unpacking, \code{zeallot}'s arrow, and \code{vadr::bind}.
#'
#' @param wrapr_private_value list of values to copy
#' @param ... argument names to write to
#' @return value passed in (invisible)
#'
#' @examples
#'
#' # named unpacking
#' # looks like assignment: DESTINATION = NAME_VALUE_USING
#' d <- data.frame(x = 1:2,
#'                 g=c('test', 'train'),
#'                 stringsAsFactors = FALSE)
#' unpack[train_set = train, test_set = test] := split(d, d$g)
#' # train_set and test_set now correctly split
#' print(train_set)
#' print(test_set)
#' rm(list = c('train_set', 'test_set'))
#'
#' # named unpacking NEWNAME = OLDNAME implicit form
#' # values are matched by name, not index
#' unpack[train, test] := split(d, d$g)
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
#' # bquote example
#' train_col_name <- 'train'
#' test_col_name <- 'test'
#' unpack(split(d, d$g), train = .(train_col_name), test = .(test_col_name))
#' print(train)
#' print(test)
#' rm(list = c('train', 'test'))
#'
#' @export
#'
unpack <- UnpackerF(object_name = "unpack")


#' @export
`:=.UnpackTarget` <- function(targets, values) {
  targets(values)
}

#' @export
`%:=%.UnpackTarget` <- function(targets, values) {
  targets(values)
}

