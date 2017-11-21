

#' Named map builder.
#'
#' Set names of right-argument to be left-argument, and return right argument.
#' Has a special case for length-1 name sets.
#' Called from \code{:=} operator.
#'
#' @param names names to set.
#' @param values values to assign names to (and return).
#' @return values with names set.
#'
#' @examples
#'
#'
#' c('a' := '4', 'b' := '5')
#' # equivalent to: c(a = '4', b = '5')
#'
#' c('a', 'b') := c('1', '2')
#' # equivalent to: c(a = '1', b = '2')
#'
#' # the important example
#' name <- 'a'
#' name := '5'
#' # equivalent to: c('a' = '5')
#'
#' # fn version:
#' #  applied when right side is {}
#' #  or when left side is of class formula.
#'
#' g <- x~y := { x + 3*y }
#' g(1,100)
#'
#' f <- ~x := x^2
#' f(7)
#'
#' @export
named_map_builder <- function(names, values) {
  # sepcial case 'a' := c('b', 'c') -> a := 'bc'
  if((length(values)>1)&&(length(names)==1)) {
    values <- do.call(paste0, as.list(values))
  }
  # main case
  names(values) <- as.character(names)
  values
}

#' @rdname named_map_builder
#' @export
`:=` <- function(names, values) {
  # check if this was a lambda assignment in disguise
  # only consider so at this checkif if RHS is {}
  # else let S3 disptach on formula pick this up
  vl <- substitute(values)
  if(is.call(vl)) {
    vl1c <- as.character(vl[[1]])
    if(vl1c=='{') {
      nv <- substitute(names)
      return(makeFunction_se(all.vars(nv), vl,  parent.frame()))
    }
  }
  # use standard S3 dispatch
  UseMethod(":=")
}


#' @export
`:=.default` <- function(names, values) {
  if(length(names)<=0) {
    return(values)
  }
  stop(paste(":=.default called with arguments of class:",
             class(names), class(values)))
}

#' @export
`:=.character` <- named_map_builder

#' @export
`:=.list` <- named_map_builder

#' @export
`:=.formula` <- function(names, values) {
  env = parent.frame()
  params <- setdiff(as.character(all.vars(substitute(names))),
                    '~')
  body <- substitute(values)
  makeFunction_se(params, body, env)
}

