

#' Pipe step operator
#'
#' S3 dispatch on class of pipe_left_arg.
#' For formal documentation please see \url{https://github.com/WinVector/wrapr/blob/master/extras/wrapr_pipe.pdf}.
#'
#' @param pipe_left_arg left argument.
#' @param pipe_right_arg substitute(pipe_right_arg) argument.
#' @param pipe_environment environment to evaluate in.
#' @param pipe_name character, name of pipe operator.
#' @return result
#'
#' @export
#'
pipe_step <- function(pipe_left_arg,
                      pipe_right_arg,
                      pipe_environment,
                      pipe_name = NULL) {
  UseMethod("pipe_step", pipe_left_arg)
}

#' Pipe step operator
#'
#' @param pipe_left_arg left argument
#' @param pipe_right_arg substitute(pipe_right_arg) argument
#' @param pipe_environment environment to evaluate in
#' @param pipe_name character, name of pipe operator.
#' @return result
#'
#' @examples
#'
#' 5 %.>% sin(.)
#'
#' @export
#'
pipe_step.default <- function(pipe_left_arg,
                              pipe_right_arg,
                              pipe_environment,
                              pipe_name = NULL) {
  # remove some exceptional cases
  if(length(pipe_right_arg)<1) {
    stop("wrapr::pipe_step.default does not allow direct piping into NULL/empty")
  }
  if(length(pipe_right_arg)==1) {
    if(is.call(pipe_right_arg)) {
      right_text <- as.character(pipe_right_arg)
      stop(paste0("wrapr::pipe_step.default does not allow direct piping into a no-argument function call expression (such as \"",
                  right_text,
                  "()\", please use ",
                  right_text, "(.))."))
    }
    stop(paste0("wrapr::pipe_step.default does not allow direct piping into scalar values such as",
                " class:" , class(pipe_right_arg), ", ",
                " type:", typeof(pipe_right_arg), "."))
  }
  if(is.call(pipe_right_arg) && (is.name(pipe_right_arg[[1]]))) {
    call_text <- as.character(pipe_right_arg[[1]])
    # mostly grabbing reserved words that are in the middle
    # of something, or try to alter control flow (like return).
    if((length(call_text)==1) &&
       call_text %in% c("else",
                        "return",
                        "in", "next", "break",
                        "TRUE", "FALSE", "NULL", "Inf", "NaN",
                        "NA", "NA_integer_", "NA_real_", "NA_complex_", "NA_character_",
                        "->", "->>", "<-", "<<-", "=", # precedence should ensure we do not see these
                        "?",
                        "...",
                        ".",
                        ";", ",",
                        "substitute", "bquote", "quote",
                        "eval", "evalq", "eval.parent", "local",
                        "force",
                        "try", "tryCatch",
                        "withCallingHandlers", "signalCondition",
                        "simpleCondition", "simpleError", "simpleWarning", "simpleMessage",
                        "withRestarts", "invokeRestart", "invokeRestartInteractively",
                        "suppressMessages", "suppressWarnings",
                        "warning", "stop")) {
      stop(paste0("wrapr::pipe_step.default does not allow direct piping into certain reserved words or control structures (such as \"",
                  call_text,
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

#' Wrapr function.
#'
#' S3 dispatch on class of pipe_right_argument.
#' For formal documentation please see \url{https://github.com/WinVector/wrapr/blob/master/extras/wrapr_pipe.pdf}.
#'
#' @param pipe_left_arg left argument.
#' @param pipe_right_arg right argument (general object, not a function).
#' @param pipe_environment environment to evaluate in.
#' @param pipe_name character, name of pipe operator.
#' @return result
#'
#' @export
#'
wrapr_function <- function(pipe_left_arg,
                           pipe_right_arg,
                           pipe_environment,
                           pipe_name = NULL) {
  UseMethod("wrapr_function", pipe_right_arg)
}

#' Wrapr function.
#'
#' S3 dispatch on tyhpe of pipe_right_argument.
#'
#' @param pipe_left_arg left argument.
#' @param pipe_right_arg right argument (general object, not a function).
#' @param pipe_environment environment to evaluate in.
#' @param pipe_name character, name of pipe operator.
#' @return result
#'
#' @examples
#'
#' f <- function() { print("execute"); 0}
#' a <- substitute({. + 1 + f()})
#' 5 %.>% a
#'
#'
#' @export
#'
wrapr_function.default <- function(pipe_left_arg,
                                   pipe_right_arg,
                                   pipe_environment,
                                   pipe_name = NULL) {
  # go to default left S3 dispatch on pipe_step()
  pipe_step(pipe_left_arg, pipe_right_arg,
            pipe_environment = pipe_environment,
            pipe_name = pipe_name)
}


#' Pipe dispatch implementation.
#'
#' @param pipe_left_arg substitute(pipe_left_arg) argument.
#' @param pipe_right_arg substitute(pipe_right_arg) argument.
#' @param pipe_environment environment to evaluate in.
#' @param pipe_name character, name of pipe operator.
#' @return result
#'
#' @noRd
#'
pipe_impl <- function(pipe_left_arg,
                      pipe_right_arg,
                      pipe_environment,
                      pipe_name = NULL) {
  # special case: parenthesis
  while(is.call(pipe_right_arg) &&
        (length(pipe_right_arg)==2) &&
        (length(as.character(pipe_right_arg[[1]]))==1) &&
        (as.character(pipe_right_arg[[1]])=="(")) {
    pipe_right_arg <- pipe_right_arg[[2]]
  }
  # force pipe_left_arg
  pipe_left_arg <- eval(pipe_left_arg,
                        envir = pipe_environment,
                        enclos = pipe_environment)
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
  # check for right-apply situations
  if(is.function(pipe_right_arg) ||
     is_name || qualified_name ||
     is_function_decl) {
    if(is_name) {
      if(as.character(pipe_right_arg)==".") {
        stop("wrapr::pipe does not allow direct piping into '.'")
      }
      pipe_right_arg <- base::get(as.character(pipe_right_arg),
                                  envir = pipe_environment,
                                  mode = "any",
                                  inherits = TRUE)
    } else if(qualified_name) {
      pipe_right_arg <- base::eval(pipe_right_arg,
                                   envir = pipe_environment,
                                   enclos = pipe_environment)
    } else if(is_function_decl) {
      pipe_right_arg <- eval(pipe_right_arg,
                            envir = pipe_environment,
                            enclos = pipe_environment)
    }
    # pipe_right_arg is now a value (as far as we are concerned)
    # special case: functions
    if(is.function(pipe_right_arg)) {
      res <- withVisible(do.call(pipe_right_arg,
                                 list(pipe_left_arg),
                                 envir = pipe_environment))
      if(res$visible) {
        return(res$value)
      } else {
        return(invisible(res$value))
      }
    }
    # S3 dispatch on right argument, surrogate function
    res <- withVisible(wrapr_function(pipe_left_arg,
                                      pipe_right_arg,
                                      pipe_environment,
                                      pipe_name))
    if(res$visible) {
      return(res$value)
    } else {
      return(invisible(res$value))
    }
  }
  # Go for standard (first argument) S3 dispatch
  res <- withVisible(pipe_step(pipe_left_arg,
                               pipe_right_arg,
                               pipe_environment,
                               pipe_name))
  if(res$visible) {
    res$value
  } else {
    invisible(res$value)
  }
}

#' Pipe operator ("dot arrow").
#'
#' Defined as roughly : \code{a \%>.\% b} ~ \code{\{ . <- a; b \};}
#' (with visible .-side effects).
#'
#' The pipe operator has a couple of special cases. First: if the right hand side is a name,
#' then we try to de-reference it and apply it as a function or surrogate function.
#'
#' The pipe operator checks for and throws an exception for a number of "pipled into
#' nothing cases" such as \code{5 \%.>\% sin()}, many of these checks can be turned
#' off by adding braces.
#'
#' For some discussion, please see \url{http://www.win-vector.com/blog/2017/07/in-praise-of-syntactic-sugar/}.
#' For some more examples, please see the package README \url{https://github.com/WinVector/wrapr}.
#' For formal documentation please see \url{https://github.com/WinVector/wrapr/blob/master/extras/wrapr_pipe.pdf}.
#' \code{\%>.\%} and \code{\%.>\%} are synonyms.
#'
#' @param pipe_left_arg left argument expression (substituted into .)
#' @param pipe_right_arg right argument expession (presumably including .)
#' @return eval(\{ . <- pipe_left_arg; pipe_right_arg \};)
#'
#' @examples
#'
#' # both should be equal:
#' cos(exp(sin(4)))
#' 4 %.>% sin(.) %.>% exp(.) %.>% cos(.)
#'
#' @export
`%.>%` <- function(pipe_left_arg, pipe_right_arg) {
  pipe_left_arg <- substitute(pipe_left_arg)
  pipe_right_arg <- substitute(pipe_right_arg)
  pipe_environment <- parent.frame()
  pipe_name <- as.character(sys.call()[[1]])
  pipe_impl(pipe_left_arg, pipe_right_arg,
            pipe_environment, pipe_name)
}

#' Pipe operator ("to dot").
#'
#' Defined as roughly : \code{a \%>.\% b} ~ \code{\{ . <- a; b \};}
#' (with visible .-side effects).
#'
#' The pipe operator has a couple of special cases. First: if the right hand side is a name,
#' then we try to de-reference it and apply it as a function or surrogate function.
#'
#' The pipe operator checks for and throws an exception for a number of "pipled into
#' nothing cases" such as \code{5 \%.>\% sin()}, many of these checks can be turned
#' off by adding braces.
#'
#' For some discussion, please see \url{http://www.win-vector.com/blog/2017/07/in-praise-of-syntactic-sugar/}.
#' For some more examples, please see the package README \url{https://github.com/WinVector/wrapr}.
#' For formal documentation please see \url{https://github.com/WinVector/wrapr/blob/master/extras/wrapr_pipe.pdf}.
#' \code{\%>.\%} and \code{\%.>\%} are synonyms.
#'
#' @param pipe_left_arg left argument expression (substituted into .)
#' @param pipe_right_arg right argument expession (presumably including .)
#' @return eval(\{ . <- pipe_left_arg; pipe_right_arg \};)
#'
#' @examples
#'
#' # both should be equal:
#' cos(exp(sin(4)))
#' 4 %>.% sin(.) %>.% exp(.) %>.% cos(.)
#'
#' @export
`%>.%` <- function(pipe_left_arg, pipe_right_arg) {
  pipe_left_arg <- substitute(pipe_left_arg)
  pipe_right_arg <- substitute(pipe_right_arg)
  pipe_environment <- parent.frame()
  pipe_name <- as.character(sys.call()[[1]])
  pipe_impl(pipe_left_arg, pipe_right_arg,
            pipe_environment, pipe_name)
}
