

#' Named map builder.
#'
#' Set names of right-argument to be left-argument, and return right argument.
#' Called from \code{:=} operator.
#'
#' @param targets names to set.
#' @param values values to assign to names (and return).
#' @return values with names set.
#'
#' @seealso \code{\link{lambda}}, \code{\link{defineLambda}}, \code{\link{makeFunction_se}}
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
#' @export
named_map_builder <- function(targets, values) {
  names <- as.character(targets)
  if(length(names)!=length(values)) {
    stop("wrapr::named_map_builder() names/values length mismatch")
  }
  names(values) <- names
  values
}




#' @rdname named_map_builder
#' @export
`:=` <- function(targets, values) {
  UseMethod(":=")
}


#' @export
`:=.character` <- named_map_builder

#' @export
`:=.name` <- named_map_builder

#' @export
`:=.symbol` <- named_map_builder

#' @export
`:=.numeric` <- named_map_builder

#' @export
`:=.list` <- named_map_builder

#' @export
`:=.factor` <- named_map_builder

#' @export
`:=.logical` <- named_map_builder





#' @rdname named_map_builder
#' @export
`%:=%` <- function(targets, values) {
  UseMethod("%:=%")
}


#' @export
`%:=%.character` <- named_map_builder

#' @export
`%:=%.name` <- named_map_builder

#' @export
`%:=%.symbol` <- named_map_builder

#' @export
`%:=%.numeric` <- named_map_builder

#' @export
`%:=%.list` <- named_map_builder

#' @export
`%:=%.factor` <- named_map_builder

#' @export
`%:=%.logical` <- named_map_builder



