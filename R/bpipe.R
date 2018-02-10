

#' Pipe step operator
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
  eval(pipe_right_arg,
       envir = pipe_environment,
       enclos = pipe_environment)
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
  pipe_right_arg
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
  # force pipe_left_arg
  pipe_left_arg <- eval(pipe_left_arg,
                        envir = pipe_environment,
                        enclos = pipe_environment)
  # eval by with pipe_left_arg's value in dot (simulates chaining)
  assign(".", pipe_left_arg,
         envir = pipe_environment,
         inherits = FALSE)
  # special case: dereference names
  if(is.name(pipe_right_arg)) {
    pipe_right_arg <- base::get(as.character(pipe_right_arg),
                                envir = pipe_environment,
                                mode = "any",
                                inherits = TRUE)
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
