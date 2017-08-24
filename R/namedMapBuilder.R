

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
#' # fn version, see makeFunction_se
#' g <- c(x,y) := { x + 3*y }
#' g(1,100)
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
  # check if this was a lambda assignment
  # only consider so if LHS is variables and RHS has {}
  nv <- substitute(names)
  vl <- substitute(values)
  isVarArray <- is.call(vl) &&
    (as.character(vl[[1]])=='{') &&
    is.language(nv) &&
    all(vapply(nv, is.name, logical(1))) &&
    (length(nv<=1) ||
       ((!any(vapply(nv, is.call, logical(1)))) &&
          as.character(nv[[1]])=='c'))
  if(isVarArray) {
    return(makeFunction_se(all.vars(nv), vl,  parent.frame()))
  }
  # use standard S3 dispatch
  rm(list= c('nv', 'vl'))
  UseMethod(":=")
}

# override as few S3 types as we reasonably need.
# deliberaterly leave default alone
# as a "good citizen".

# #' @export
# `:=.default` <- named_map_builder

#' @export
`:=.character` <- named_map_builder

#' @export
`:=.list` <- named_map_builder

#' #' @export
#' `:=.name` <- named_map_builder


