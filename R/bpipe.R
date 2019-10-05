

#' S3 dispatch on class of pipe_left_arg.
#'
#' For formal documentation please see \url{https://github.com/WinVector/wrapr/blob/master/extras/wrapr_pipe.pdf}.
#'
#' @param pipe_left_arg left argument.
#' @param pipe_right_arg substitute(pipe_right_arg) argument.
#' @param pipe_environment environment to evaluate in.
#' @param left_arg_name name, if not NULL name of left argument.
#' @param pipe_string character, name of pipe operator.
#' @param right_arg_name name, if not NULL name of right argument.
#' @return result
#'
#' @seealso \code{\link{apply_left.default}}
#'
#' @examples
#'
#' apply_left.character <- function(pipe_left_arg,
#'                                  pipe_right_arg,
#'                                  pipe_environment,
#'                                  left_arg_name,
#'                                  pipe_string,
#'                                  right_arg_name) {
#'   if(is.language(pipe_right_arg)) {
#'     wrapr::apply_left_default(pipe_left_arg,
#'                               pipe_right_arg,
#'                               pipe_environment,
#'                               left_arg_name,
#'                               pipe_string,
#'                               right_arg_name)
#'   } else {
#'     paste(pipe_left_arg, pipe_right_arg)
#'   }
#' }
#' setMethod(
#'   wrapr::apply_right_S4,
#'   signature = c(pipe_left_arg = "character", pipe_right_arg = "character"),
#'   function(pipe_left_arg,
#'            pipe_right_arg,
#'            pipe_environment,
#'            left_arg_name,
#'            pipe_string,
#'            right_arg_name) {
#'     paste(pipe_left_arg, pipe_right_arg)
#'   })
#'
#' "a" %.>% 5 %.>% 7
#'
#' "a" %.>% toupper(.)
#'
#' q <- "z"
#' "a" %.>% q
#'
#'
#' @export
#'
apply_left <- function(pipe_left_arg,
                       pipe_right_arg,
                       pipe_environment,
                       left_arg_name,
                       pipe_string,
                       right_arg_name) {
  force(pipe_environment)
  UseMethod("apply_left", pipe_left_arg)
}

# things we don't want to piple into
forbidden_pipe_destination_names <- c("else",
                                      "return",
                                      "stop", "warning",
                                      "in", "next", "break",
                                      "TRUE", "FALSE", "NULL", "Inf", "NaN",
                                      "NA", "NA_integer_", "NA_real_", "NA_complex_", "NA_character_",
                                      "->", "->>", "<-", "<<-", "=", # precedence should ensure we do not see these
                                      "?",
                                      "...",
                                      ".",
                                      ";", ",")

#' S3 dispatch on class of pipe_left_arg.
#'
#' Place evaluation of left argument in \code{.} and then evaluate right argument.
#'
#' @param pipe_left_arg left argument
#' @param pipe_right_arg substitute(pipe_right_arg) argument
#' @param pipe_environment environment to evaluate in
#' @param left_arg_name name, if not NULL name of left argument.
#' @param pipe_string character, name of pipe operator.
#' @param right_arg_name name, if not NULL name of right argument.
#' @return result
#'
#' @seealso \code{\link{apply_left}}
#'
#' @examples
#'
#' 5 %.>% sin(.)
#'
#' @export
#'
apply_left_default <- function(pipe_left_arg,
                               pipe_right_arg,
                               pipe_environment,
                               left_arg_name,
                               pipe_string,
                               right_arg_name) {
  force(pipe_environment)
  # remove some exceptional cases
  if(length(pipe_right_arg)<1) {
    stop("wrapr::apply_left.default does not allow direct piping into NULL/empty")
  }
  # check for piping into constants such as 4 %.>% 5
  if(!is.language(pipe_right_arg)) {
    stop(paste("wrapr::apply_left.default does not allow piping into obvious concrete right-argument (clearly can't depend on left argument):\n",
               ifelse(is.null(left_arg_name), "", left_arg_name), paste(class(pipe_left_arg), collapse = ", "), "\n",
               ifelse(is.null(right_arg_name), "", right_arg_name), paste(class(pipe_right_arg), collapse = ", ")),
         call. = FALSE)
  }
  if(is.call(pipe_right_arg) && (is.name(pipe_right_arg[[1]]))) {
    call_text <- as.character(pipe_right_arg[[1]])
    # mostly grabbing reserved words that are in the middle
    # of something, or try to alter control flow (like return).
    if(isTRUE(call_text %in% forbidden_pipe_destination_names)) {
      stop(paste0("to reduce surprising execution behavior wrapr::apply_left.default does not allow direct piping into some expressions (such as \"",
                  wrapr_deparse(pipe_right_arg),
                  "\")."))
    }
  }
  # eval by with pipe_left_arg's value in dot (simulates chaining)
  assign(".", pipe_left_arg,
         envir = pipe_environment,
         inherits = FALSE)
  eval(pipe_right_arg,
       envir = pipe_environment,
       enclos = pipe_environment)
}


#' S3 dispatch on class of pipe_left_arg.
#'
#' Place evaluation of left argument in \code{.} and then evaluate right argument.
#'
#' @param pipe_left_arg left argument
#' @param pipe_right_arg substitute(pipe_right_arg) argument
#' @param pipe_environment environment to evaluate in
#' @param left_arg_name name, if not NULL name of left argument.
#' @param pipe_string character, name of pipe operator.
#' @param right_arg_name name, if not NULL name of right argument.
#' @return result
#'
#' @seealso \code{\link{apply_left}}
#'
#' @examples
#'
#' 5 %.>% sin(.)
#'
#' @export
#'
apply_left.default <- function(pipe_left_arg,
                               pipe_right_arg,
                               pipe_environment,
                               left_arg_name,
                               pipe_string,
                               right_arg_name) {
  force(pipe_environment)
  apply_left_default(pipe_left_arg = pipe_left_arg,
                     pipe_right_arg = pipe_right_arg,
                     pipe_environment = pipe_environment,
                     left_arg_name = left_arg_name,
                     pipe_string = pipe_string,
                     right_arg_name = right_arg_name)
}





#' S3 dispatch on class of pipe_right_argument.
#'
#' Triggered if right hand side of pipe stage was a name that does not resolve to a function.
#' For formal documentation please see \url{https://github.com/WinVector/wrapr/blob/master/extras/wrapr_pipe.pdf}.
#'
#'
#' @param pipe_left_arg left argument
#' @param pipe_right_arg right argument
#' @param pipe_environment environment to evaluate in
#' @param left_arg_name name, if not NULL name of left argument.
#' @param pipe_string character, name of pipe operator.
#' @param right_arg_name name, if not NULL name of right argument.
#' @return result
#'
#' @seealso \code{\link{apply_left}}, \code{\link{apply_right_S4}}
#'
#' @examples
#'
#' # simulate a function pointer
#' apply_right.list <- function(pipe_left_arg,
#'                              pipe_right_arg,
#'                              pipe_environment,
#'                              left_arg_name,
#'                              pipe_string,
#'                              right_arg_name) {
#'   pipe_right_arg$f(pipe_left_arg)
#' }
#'
#' f <- list(f=sin)
#' 2 %.>% f
#' f$f <- cos
#' 2 %.>% f
#'
#' @export
#'
apply_right <- function(pipe_left_arg,
                        pipe_right_arg,
                        pipe_environment,
                        left_arg_name,
                        pipe_string,
                        right_arg_name) {
  force(pipe_environment)
  UseMethod("apply_right", pipe_right_arg)
}


#' Default apply_right implementation.
#'
#' Default apply_right implementation: S4 dispatch to apply_right_S4.
#'
#' @param pipe_left_arg left argument
#' @param pipe_right_arg pipe_right_arg argument
#' @param pipe_environment environment to evaluate in
#' @param left_arg_name name, if not NULL name of left argument.
#' @param pipe_string character, name of pipe operator.
#' @param right_arg_name name, if not NULL name of right argument.
#' @return result
#'
#' @seealso \code{\link{apply_left}}, \code{\link{apply_right}}, \code{\link{apply_right_S4}}
#'
#' @examples
#'
#' # simulate a function pointer
#' apply_right.list <- function(pipe_left_arg,
#'                              pipe_right_arg,
#'                              pipe_environment,
#'                              left_arg_name,
#'                              pipe_string,
#'                              right_arg_name) {
#'   pipe_right_arg$f(pipe_left_arg)
#' }
#'
#' f <- list(f=sin)
#' 2 %.>% f
#' f$f <- cos
#' 2 %.>% f
#'
#' @export
#'
apply_right.default <- function(pipe_left_arg,
                                pipe_right_arg,
                                pipe_environment,
                                left_arg_name,
                                pipe_string,
                                right_arg_name) {
  force(pipe_environment)
  apply_right_S4(pipe_left_arg = pipe_left_arg,
                 pipe_right_arg = pipe_right_arg,
                 pipe_environment = pipe_environment,
                 left_arg_name = left_arg_name,
                 pipe_string = pipe_string,
                 right_arg_name = right_arg_name)
}

#' S4 dispatch method for apply_right.
#'
#' Intended to be generic on first two arguments.
#'
#' @param pipe_left_arg left argument
#' @param pipe_right_arg pipe_right_arg argument
#' @param pipe_environment environment to evaluate in
#' @param left_arg_name name, if not NULL name of left argument.
#' @param pipe_string character, name of pipe operator.
#' @param right_arg_name name, if not NULL name of right argument.
#' @return result
#'
#' @seealso \code{\link{apply_left}}, \code{\link{apply_right}}
#'
#' @examples
#'
#' a <- data.frame(x = 1)
#' b <- data.frame(x = 2)
#'
#' # a %.>% b # will (intentionally) throw
#'
#' setMethod(
#'   "apply_right_S4",
#'   signature("data.frame", "data.frame"),
#'   function(pipe_left_arg,
#'            pipe_right_arg,
#'            pipe_environment,
#'            left_arg_name,
#'            pipe_string,
#'            right_arg_name) {
#'     rbind(pipe_left_arg, pipe_right_arg)
#'   })
#'
#'
#' a %.>% b # should equal data.frame(x = c(1, 2))
#'
#' @export
#'
apply_right_S4 <- function(pipe_left_arg,
                           pipe_right_arg,
                           pipe_environment,
                           left_arg_name,
                           pipe_string,
                           right_arg_name) {
  force(pipe_environment)
  # # go to default left S3 dispatch on apply_left()
  # apply_left(pipe_left_arg = pipe_left_arg,
  #            pipe_right_arg = pipe_right_arg,
  #            pipe_environment = pipe_environment,
  #            left_arg_name = left_arg_name,
  #            pipe_string = pipe_string,
  #            right_arg_name = right_arg_name)
  stop(paste("wrapr::apply_right_S4 default called with classes:\n",
             ifelse(is.null(left_arg_name), "", left_arg_name), paste(class(pipe_left_arg), collapse = ", "), "\n",
             ifelse(is.null(right_arg_name), "", right_arg_name), paste(class(pipe_right_arg), collapse = ", "), "\n",
             " must have a more specific S4 method defined to dispatch\n"),
       call. = FALSE)
}

#' @importFrom methods setGeneric
NULL

# lash in S4 dispatch
methods::setGeneric(
  name = "apply_right_S4")


#' Pipe dispatch implementation.
#'
#' This is a helper for implementing additional pipes.
#'
#' @param pipe_left_arg substitute(pipe_left_arg) argument.
#' @param pipe_right_arg substitute(pipe_right_arg) argument.
#' @param pipe_environment environment to evaluate in.
#' @param pipe_string character, name of pipe operator.
#' @return result
#'
#' @keywords internal
#'
#' @export
#'
pipe_impl <- function(pipe_left_arg,
                      pipe_right_arg,
                      pipe_environment,
                      pipe_string = NULL) {
  # make sure environment is available
  force(pipe_environment)
  # special case: parenthesis
  while(is.call(pipe_right_arg) &&
        (length(pipe_right_arg)==2) &&
        (length(as.character(pipe_right_arg[[1]]))==1) &&
        (as.character(pipe_right_arg[[1]])=="(")) {
    pipe_right_arg <- pipe_right_arg[[2]]
  }
  # capture names
  left_arg_name <- NULL
  if(is.name(pipe_left_arg)) {
    left_arg_name <- pipe_left_arg
  }
  right_arg_name <- NULL
  if(is.name(pipe_right_arg)) {
    right_arg_name <- pipe_right_arg
  }
  # special case: name
  is_name <- is.name(pipe_right_arg)
  # special case: dereference names
  qualified_name <- is.call(pipe_right_arg) &&
    (length(pipe_right_arg)==3) &&
    (length(as.character(pipe_right_arg[[1]]))==1) &&
    (as.character(pipe_right_arg[[1]]) %in% c("::", ":::", "$", "[[", "[", "@")) &&
    (is.name(pipe_right_arg[[2]])) &&
    (as.character(pipe_right_arg[[2]])!=".") &&
    (is.name(pipe_right_arg[[3]]) || is.character(pipe_right_arg[[3]])) &&
    (as.character(pipe_right_arg[[3]])!=".")
  # special case: anonymous funciton decl
  is_function_decl <- is.call(pipe_right_arg) &&
    (length(as.character(pipe_right_arg[[1]]))==1) &&
    (as.character(pipe_right_arg[[1]])=="function")
  # special-case .() on RHS
  dot_paren <- is.call(pipe_right_arg) &&
    (length(as.character(pipe_right_arg[[1]]))==1) &&
    (as.character(pipe_right_arg[[1]])[[1]]==".")
  # check for right-apply situations
  if(is.function(pipe_right_arg) ||
     is_name || qualified_name ||
     is_function_decl || dot_paren) {
    if(is_name) {
      if(as.character(pipe_right_arg) %in% forbidden_pipe_destination_names) {
        stop(paste("to reduce surprising behavior wrapr::pipe does not allow direct piping into some names, such as",
                   as.character(pipe_right_arg)))
      }
      pipe_right_arg <- base::get(as.character(pipe_right_arg),
                                  envir = pipe_environment,
                                  mode = "any",
                                  inherits = TRUE)
    } else if(qualified_name || is_function_decl) {
      pipe_right_arg <- base::eval(pipe_right_arg,
                                   envir = pipe_environment,
                                   enclos = pipe_environment)
    } else if(dot_paren) {
      pipe_right_arg <- eval(pipe_right_arg[[2]],
                            envir = pipe_environment,
                            enclos = pipe_environment)
    }
    # pipe_right_arg is now a value (as far as we are concerned)
    # special case: functions
    if(is.function(pipe_right_arg)) {
      if(!is.null(left_arg_name)) {
        # try to pass name forward to function
        # support NSE in functions, but don't encourage it in expressions
        res <- withVisible(do.call(pipe_right_arg,
                                   list(left_arg_name),
                                   envir = pipe_environment))
      } else {
        # force pipe_left_arg
        pipe_left_arg <- eval(pipe_left_arg,
                              envir = pipe_environment,
                              enclos = pipe_environment)
        res <- withVisible(do.call(pipe_right_arg,
                                   list(pipe_left_arg),
                                   envir = pipe_environment))
      }
      if(res$visible) {
        return(res$value)
      } else {
        return(invisible(res$value))
      }
    }
    # force pipe_left_arg
    pipe_left_arg <- eval(pipe_left_arg,
                          envir = pipe_environment,
                          enclos = pipe_environment)
    # S3 dispatch on right argument, surrogate function
    res <- withVisible(apply_right(pipe_left_arg,
                                   pipe_right_arg,
                                   pipe_environment,
                                   left_arg_name,
                                   pipe_string,
                                   right_arg_name))
    if(res$visible) {
      return(res$value)
    } else {
      return(invisible(res$value))
    }
  }
  # force pipe_left_arg
  pipe_left_arg <- eval(pipe_left_arg,
                        envir = pipe_environment,
                        enclos = pipe_environment)
  # Go for standard (first argument) S3 dispatch
  res <- withVisible(apply_left(pipe_left_arg,
                                pipe_right_arg,
                                pipe_environment,
                                left_arg_name,
                                pipe_string,
                                right_arg_name))
  if(res$visible) {
    res$value
  } else {
    invisible(res$value)
  }
}

#' Pipe operator ("dot arrow", "dot pipe" or "dot arrow pipe").
#'
#' Defined as roughly : \code{a \%>.\% b} ~ \code{\{ . <- a; b \};}
#' (with visible .-side effects).
#'
#' The pipe operator has a couple of special cases. First: if the right hand side is a name,
#' then we try to de-reference it and apply it as a function or surrogate function.
#'
#' The pipe operator checks for and throws an exception for a number of "piped into
#' nothing cases" such as \code{5 \%.>\% sin()}, many of these checks can be turned
#' off by adding braces.
#'
#' For some discussion, please see \url{http://www.win-vector.com/blog/2017/07/in-praise-of-syntactic-sugar/}.
#' For some more examples, please see the package README \url{https://github.com/WinVector/wrapr}.
#' For formal documentation please see \url{https://github.com/WinVector/wrapr/blob/master/extras/wrapr_pipe.pdf}.
#' For a base-R step-debuggable pipe please try the Bizarro Pipe \url{http://www.win-vector.com/blog/2017/01/using-the-bizarro-pipe-to-debug-magrittr-pipelines-in-r/}.
#' \code{\%>.\%} and \code{\%.>\%} are synonyms.
#'
#' The dot arrow pipe has S3/S4 dispatch (please see \url{https://journal.r-project.org/archive/2018/RJ-2018-042/index.html}).
#' However as the right-hand side of the pipe is normally held unevaluated, we don't know the type except in special
#' cases (such as the rigth-hand side being referred to by a name or variable).  To force the evaluation of a pipe term,
#' simply wrap it in .().
#'
#' @param pipe_left_arg left argument expression (substituted into .)
#' @param pipe_right_arg right argument expression (presumably including .)
#' @return eval(\{ . <- pipe_left_arg; pipe_right_arg \};)
#'
#' @examples
#'
#' # both should be equal:
#' cos(exp(sin(4)))
#' 4 %.>% sin(.) %.>% exp(.) %.>% cos(.)
#'
#' f <- function() { sin }
#' # returns f() ignoring dot, not what we want
#' 5 %.>% f()
#' # evaluates f() early then evaluates result with .-substitution rules
#' 5 %.>% .(f())
#'
#' @name dot_arrow
NULL

#' @describeIn dot_arrow dot arrow
#' @export
`%.>%` <- function(pipe_left_arg, pipe_right_arg) {
  pipe_left_arg <- substitute(pipe_left_arg)
  pipe_right_arg <- substitute(pipe_right_arg)
  pipe_environment <- parent.frame()
  pipe_string <- as.character(sys.call()[[1]])
  pipe_impl(pipe_left_arg, pipe_right_arg,
            pipe_environment, pipe_string)
}

#' @describeIn dot_arrow alias for dot arrow
#' @export
`%>.%` <- function(pipe_left_arg, pipe_right_arg) {
  pipe_left_arg <- substitute(pipe_left_arg)
  pipe_right_arg <- substitute(pipe_right_arg)
  pipe_environment <- parent.frame()
  pipe_string <- as.character(sys.call()[[1]])
  pipe_impl(pipe_left_arg, pipe_right_arg,
            pipe_environment, pipe_string)
}


#' @describeIn dot_arrow alias for dot arrow
#' @export
`%.%` <- function(pipe_left_arg, pipe_right_arg) {
  pipe_left_arg <- substitute(pipe_left_arg)
  pipe_right_arg <- substitute(pipe_right_arg)
  pipe_environment <- parent.frame()
  pipe_string <- as.character(sys.call()[[1]])
  pipe_impl(pipe_left_arg, pipe_right_arg,
            pipe_environment, pipe_string)
}
