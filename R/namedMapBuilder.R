

#' Named map builder.
#'
#' Set names of right-argument to be left-argument, and return right argument.
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
#' f <- x := { sqrt(x) }
#' f(7)
#'
#' @export
named_map_builder <- function(names, values) {
  names <- as.character(names)
  if(length(names)!=length(values)) {
    stop("wrapr::named_map_builder() names/values length mismatch")
  }
  names(values) <- names
  values
}

#' @rdname named_map_builder
#' @export
`:=` <- function(names, values) {
  # check if this was a lambda assignment in disguise
  # only consider so at this checkif if RHS is {}
  # else let S3 disptach on formula pick this up
  res <- early_tries(substitute(names), substitute(values), values)
  if(!is.null(res)) {
    return(res)
  }
  # use standard S3 dispatch
  UseMethod(":=")
}

# pretty much assume vector name assignment, or function definiton
# need this to grab unevaluated cases
early_tries <- function(nm, vl, values) {
  # see if we should force function def mode
  couldBeFn <-
    (is.name(nm) && (length(nm)==1)) ||
    (is.character(nm) && (length(nm)==1)) ||
    (is.call(nm) && (as.character(nm[[1]]) %in% c("~", "c", "list")))
  # our stated rule: formula on left or brace on right
  shouldBeFn <-
    (is.call(nm) && (as.character(nm[[1]])=="~")) ||
    (is.call(vl) && (as.character(vl[[1]])=="{"))
  if(couldBeFn && shouldBeFn) {
    if(is.call(nm) && (as.character(nm[[1]]) %in% c("c", "list"))) {
      vars <- as.character(nm[seq_len(length(nm)-1)+1])
    } else if(is.name(nm) || is.character(nm)) {
      vars <- as.character(nm)
    } else {
      # assume formula
      vars <- setdiff(as.character(all.vars(nm)), "~")
    }
    return(makeFunction_se(vars, vl, parent.frame()))
  }
  NULL # continue in the normal way
}

#' @export
`:=.character` <- named_map_builder

#' @export
`:=.numeric` <- named_map_builder

#' @export
`:=.list` <- named_map_builder

