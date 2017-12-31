
pipe_impl <- function(pipe_left_arg, pipe_right_arg, pipe_environment) {
  # force pipe_left_arg, by left-associativity "pipe_left_arg" may be a pipe
  # sequence itself.
  # We are not bothering to capture that as a list, just letting
  # R's calling sequence take us to those pieces.
  pipe_left_arg <- eval(pipe_left_arg,
                        envir = pipe_environment,
                        enclos = pipe_environment)
  # eval by with pipe_left_arg's value in dot (simulates chaining)
  assign(".", pipe_left_arg,
         envir= pipe_environment,
         inherits= FALSE)
  # special case: dereference names
  if(is.name(pipe_right_arg)) {
    v <- base::mget(as.character(pipe_right_arg),
                    envir = pipe_environment,
                    ifnotfound = list(NULL),
                    inherits = TRUE)[[1]]
    if(!is.null(v)) {
      pipe_right_arg <- v
    }
  }
  # special case: functions
  if(is.function(pipe_right_arg)) {
    return(do.call(pipe_right_arg,
                   list(pipe_left_arg),
                   envir = pipe_environment))
  }
  # special case: look for wrapr_applicable objects
  if((!is.atomic(pipe_right_arg)) &&
     ("wrapr_applicable" %in% class(pipe_right_arg))) {
    f <- pipe_right_arg$wrapr_function
    if((!is.null(f)) && (is.function(f))) {
      return(do.call(f,
                     list(pipe_left_arg = pipe_left_arg,
                          pipe_right_arg = pipe_right_arg,
                          pipe_environment = pipe_environment),
                     envir = pipe_environment))
    }
  }
  eval(pipe_right_arg,
       envir = pipe_environment,
       enclos = pipe_environment)
}

#' Pipe operator ("dot arrow").
#'
#' Defined as roughly : \code{a \%>.\% b} ~ \code{\{ . <- a; b \};}
#' (with visible .-side effects).
#'
#' The pipe operator has a couple of special cases. First: if the right hand side is a name,
#' then we try to de-reference it.  Second: if the right-hand side includes the class decleration
#' "wrapr_applicable" and has a field named "wrapr_applicable" that is a function, then
#' we apply this function to the first and second arguments of the pipe.
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
  pipe_impl(pipe_left_arg, pipe_right_arg, pipe_environment)
}

#' Pipe operator ("to dot").
#'
#' Defined as roughly : \code{a \%>.\% b} ~ \code{\{ . <- a; b \};}
#' (with visible .-side effects).
#'
#' The pipe operator has a couple of special cases. First: if the right hand side is a name,
#' then we try to de-reference it.  Second: if the right-hand side includes the class decleration
#' "wrapr_applicable" and has a field named "wrapr_applicable" that is a function, then
#' we apply this function to the first and second arguments of the pipe.
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
  pipe_impl(pipe_left_arg, pipe_right_arg, pipe_environment)
}
