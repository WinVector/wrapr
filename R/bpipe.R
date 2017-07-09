

#' Pipe-like operator ("happy arrow").
#'
#' Defined as: \code{a \%:>\% b} roughly ~ \code{\{ . <- a; b \};}
#' (with visible .-side effects).
#' Please see \url{http://www.win-vector.com/blog/2017/07/in-praise-of-syntactic-sugar/}.
#'
#' @param pipe_left_arg left argument expression (substituted into .)
#' @param pipe_right_arg right argument expession (presumably including .)
#' @return eval(\{ . <- pipe_left_arg; pipe_right_arg \};)
#'
#' @examples
#'
#' # both should be equal:
#' cos(exp(sin(4)))
#' 4 %:>% sin(.) %:>% exp(.) %:>% cos(.)
#'
#' @export
`%:>%` <- function(pipe_left_arg, pipe_right_arg) {
  pipe_left_arg <- substitute(pipe_left_arg)
  pipe_right_arg <- substitute(pipe_right_arg)
  pipe_environment <- parent.frame()
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
  eval(pipe_right_arg,
            envir=pipe_environment,
            enclos=pipe_environment)
}


#' Pipe-like operator ("dot arrow").
#'
#' Defined as: \code{a \%.>\% b} roughly ~ \code{\{ . <- a; b \};}
#' (though, we try to stop .-side effects).
#' Please see \url{http://www.win-vector.com/blog/2017/07/in-praise-of-syntactic-sugar/}.
#'
#' @param pipe_left_arg left argument expression (substituted into .)
#' @param pipe_right_arg right argument expession (presumably including .)
#' @return eval(\{ . <- pipe_left_arg; pipe_right_arg \};) (with .-side effects removed)
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
  # force pipe_left_arg, by left-associativity "pipe_left_arg" may be a pipe
  # sequence itself.
  # We are not bothering to capture that as a list, just letting
  # R's calling sequence take us to those pieces.
  pipe_left_arg <- eval(pipe_left_arg,
                        envir = pipe_environment,
                        enclos = pipe_environment)
  # check for a previous . (try to avoid visible side-effects)
  hadDot <- exists(".", envir = pipe_environment,
                   inherits = FALSE)
  prevDot <- NULL
  if(hadDot) {
    prevDot <- get(".", envir = pipe_environment,
                   inherits = FALSE)
  }
  # eval by with pipe_left_arg's value in dot (simulates chaining)
  assign(".", pipe_left_arg,
         envir= pipe_environment,
         inherits= FALSE)
  pipe_result <- eval(pipe_right_arg,
                      envir=pipe_environment,
                      enclos=pipe_environment)
  if(hadDot) {
    assign(".", prevDot,
           envir= pipe_environment,
           inherits= FALSE)
  } else {
    rm(list=".",
       envir= pipe_environment,
       inherits= FALSE)
  }
  pipe_result
}


