

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
#' @export
#'
pipe_step.default <- function(pipe_left_arg,
                              pipe_right_arg,
                              pipe_environment,
                              pipe_name = NULL) {
  force(pipe_left_arg)
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
#' @param pipe_right_arg right argument.
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
#' @param pipe_right_arg right argument.
#' @param pipe_environment environment to evaluate in.
#' @param pipe_name character, name of pipe operator.
#' @return result
#'
#' @export
#'
wrapr_function.default <- function(pipe_left_arg,
                                   pipe_right_arg,
                                   pipe_environment,
                                   pipe_name = NULL) {
  # not a example, but follows we are treating
  # pipe_right_arg as a value (as it is not a function when
  # we get to here).
  # Also this default matches the pipe_step() default impl.
  force(pipe_left_arg)
  eval(pipe_right_arg,
       envir = pipe_environment,
       enclos = pipe_environment)
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
  # remove some exceptional cases
  if(length(pipe_right_arg)<1) {
    stop("wrapr::pipe does not allow direct piping into NULL/empty")
  }
  if(length(pipe_right_arg)<=1) {
    if(is.call(pipe_right_arg)) {
      # empty calls (easy to detect no-. case)
      call_name <- as.character(pipe_right_arg[[1]])
      stop(paste0("wrapr::pipe does not allow direct piping into a no-argument function call expression (such as \"",
                  call_name,
                  "()\" please use ",
                  call_name, "(.))."))
    }
    # don't index as argument may be a symbol or character already
    if(as.character(pipe_right_arg)==".") {
      stop("wrapr::pipe does not allow direct piping into \".\"")
    }
    if((!is.language(pipe_right_arg)) &&
       (!is.call(pipe_right_arg)) &&
       (!is.symbol(pipe_right_arg)) &&
       (!is.function(pipe_right_arg)) &&
       ((length(class(pipe_right_arg))<1) ||
        (length(class(pipe_right_arg))<2) &&
        (class(pipe_right_arg) %in% c("numeric", "character",
                                      "logical", "integer",
                                      "raw", "complex")))) {
      stop(paste0("wrapr::pipe does not allow direct piping into simple values such as",
                  " class:" , class(pipe_right_arg), ", ",
                  " type:", typeof(pipe_right_arg), "."))
    }
  }
  if(is.call(pipe_right_arg)) {
    call_name <- as.character(pipe_right_arg[[1]])
    # mostly grabbing reserved words that are in the middle
    # of something, or try to alter control flow (like return).
    if(call_name %in% c("else",
                        "function",
                        "return",
                        "in", "next", "break",
                        "TRUE", "FALSE", "NULL", "Inf", "NaN",
                        "NA", "NA_integer_", "NA_real_", "NA_complex_", "NA_character_",
                        "->", "->>", "<-", "<<-", "=", # precedence should ensure we do not see these
                        "?",
                        "...",
                        ".",
                        ";", ",")) {
      stop(paste0("wrapr::pipe does not allow direct piping into reserved word or control structure (such as \"",
                  call_name,
                  "\")."))
    }
  }
  # force pipe_left_arg
  pipe_left_arg <- eval(pipe_left_arg,
                        envir = pipe_environment,
                        enclos = pipe_environment)
  # eval by with pipe_left_arg's value in dot (simulates chaining)
  assign(".", pipe_left_arg,
         envir = pipe_environment,
         inherits = FALSE)
  # special case: dereference names
  qualified_name <- is.call(pipe_right_arg) &&
    (length(pipe_right_arg)==3) &&
    (as.character(pipe_right_arg[[1]]) %in% c("::", ":::", "$", "[[", "[", "@")) &&
    (is.name(pipe_right_arg[[2]])) &&
    (as.character(pipe_right_arg[[2]])!=".") &&
    (is.name(pipe_right_arg[[3]]) || is.character(pipe_right_arg[[3]])) &&
    (as.character(pipe_right_arg[[3]])!=".")
  is_name <- is.name(pipe_right_arg)
  if(is_name || qualified_name) {
    if(is_name) {
      pipe_right_arg <- base::get(as.character(pipe_right_arg),
                                  envir = pipe_environment,
                                  mode = "any",
                                  inherits = TRUE)
    } else if(qualified_name) {
      pipe_right_arg <- base::eval(pipe_right_arg,
                                   envir = pipe_environment,
                                   enclos = pipe_environment)
    }
    # pipe_right_arg is now a value (as far as we are concerned)
    # special case: functions
    if(is.function(pipe_right_arg)) {
      res <- do.call(pipe_right_arg,
                     list(pipe_left_arg),
                     envir = pipe_environment)
      return(res)
    }
    # S3 dispatch on right argument, surrogate function
    res <- wrapr_function(pipe_left_arg,
                          pipe_right_arg,
                          pipe_environment,
                          pipe_name)
    return(res)
  }
  # Go for standard (first argument) S3 dispatch
  res <- pipe_step(pipe_left_arg,
                   pipe_right_arg,
                   pipe_environment,
                   pipe_name)
  res
}

#' Pipe operator ("dot arrow").
#'
#' Defined as roughly : \code{a \%>.\% b} ~ \code{\{ . <- a; b \};}
#' (with visible .-side effects).
#'
#' The pipe operator has a couple of special cases. First: if the right hand side is a name,
#' then we try to de-reference it and apply it as a function or surrogate function.
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
#' For some discussion, please see \url{http://www.win-vector.com/blog/2017/07/in-praise-of-syntactic-sugar/}.
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
