

#' Quoting version of c() array concatenate.
#'
#' The qc() function is intended to help quote user inputs.
#'
#' qc() a convenience function allowing the user to elide
#' excess quotation marks.  It quotes its arguments instead
#' of evaluating them, except in the case of a nested
#' call to qc() or c().  Please see the examples for
#' typical uses both for named and un-named character vectors.
#'
#'
#' qc() uses bquote() .() quasiquotation escaping notation.
#'
#'
#' @param ... items to place into an array
#' @param .wrapr_private_var_env environment to evaluate in
#' @return quoted array of character items
#'
#' @seealso \code{\link{qe}}, \code{\link{qae}}, \code{\link[base]{bquote}}
#'
#' @examples
#'
#' a <- "x"
#'
#' qc(a) # returns the string "a" (not "x")
#'
#' qc(.(a)) # returns the string "x" (not "a")
#'
#' qc(.(a) := a) # returns c("x" = "a")
#'
#' qc("a") # return the string "a" (not "\"a\"")
#'
#' qc(sin(x))  # returns the string "sin(x)"
#'
#' qc(a, qc(b, c)) # returns c("a", "b", "c")
#'
#' qc(a, c("b", "c")) # returns c("a", "b", "c")
#'
#' qc(x=a, qc(y=b, z=c)) # returns c(x="a", y="b", z="c")
#'
#' qc('x'='a', wrapr::qc('y'='b', 'z'='c')) # returns c(x="a", y="b", z="c")
#'
#' c(a = c(a="1", b="2")) # returns c(a.a = "1", a.b = "2")
#' qc(a = c(a=1, b=2)) # returns c(a.a = "1", a.b = "2")
#' qc(a := c(a=1, b=2)) # returns c(a.a = "1", a.b = "2")
#'
#'
#' @export
#'
qc <- function(..., .wrapr_private_var_env = parent.frame()) {
  # invariant: returns are always character vectors
  force(.wrapr_private_var_env)
  #.wrapr_private_var_args <- substitute(list(...))
  .wrapr_private_var_args <- do.call(bquote, list(substitute(list(...)),
                                                   where = .wrapr_private_var_env),
                                     envir = .wrapr_private_var_env)
  if(length(.wrapr_private_var_args)<=1) {
    return(character(0))
  }
  .wrapr_private_var_names <- names(.wrapr_private_var_args)
  .wrapr_private_var_res <- lapply(
    2:length(.wrapr_private_var_args),
    function(.wrapr_private_var_i) {
      .wrapr_private_var_ei <- .wrapr_private_var_args[[.wrapr_private_var_i]]
      if(missing(.wrapr_private_var_ei)) {
        stop("saw missing argument to qc, the cause is often an extra comma in the argument list")
      }
      .wrapr_private_var_ni <- NULL
      if(.wrapr_private_var_i<=length(.wrapr_private_var_names)) {
        .wrapr_private_var_ni <- .wrapr_private_var_names[[.wrapr_private_var_i]]
        if(nchar(.wrapr_private_var_ni)<=0) {
          .wrapr_private_var_ni <- NULL
        }
      }
      if(is.name(.wrapr_private_var_ei)) {
        # names are scalars
        .wrapr_private_var_ei <- as.character(.wrapr_private_var_ei)
        if(!is.null(.wrapr_private_var_ni)) {
          names(.wrapr_private_var_ei) <- .wrapr_private_var_ni
        }
        return(.wrapr_private_var_ei)
      }
      if(is.language(.wrapr_private_var_ei)) {
        if(is.call(.wrapr_private_var_ei)) {
          .wrapr_private_var_fnname <- deparse(.wrapr_private_var_ei[[1]])
          .wrapr_private_var_fnname <- gsub("[[:space:]]+", "", .wrapr_private_var_fnname)
          if(isTRUE(.wrapr_private_var_fnname %in%
             c(":=", "%:=%"))) {
            v <- do.call(qc, c(list(.wrapr_private_var_ei[[3]]),
                               list(.wrapr_private_var_env = .wrapr_private_var_env)),
                         envir = .wrapr_private_var_env)
            nms <- do.call(qc, c(list(.wrapr_private_var_ei[[2]]),
                                 list(.wrapr_private_var_env = .wrapr_private_var_env)),
                           envir = .wrapr_private_var_env)
            if((length(nms)==1)&&(length(nms)<length(v))) {
              nms <- nms[rep(1, length(v))]
            }
            if(length(names(v))>0) {
              nms <- paste(nms, names(v), sep=".")
            }
            names(v) <- nms
            return(v)
          }
          if(isTRUE(.wrapr_private_var_fnname %in%
                    c("qc", "wrapr::qc",
                      "c", "base::c",
                      "%c%", "`%c%`",
                      "%qc%", "`%qc%`"))) {
            # this is the recursive case qc('x'='a', qc('y'='b', 'z'='c'))
            .wrapr_private_var_ei <- eval(.wrapr_private_var_ei,
                                          envir = .wrapr_private_var_env,
                                          enclos = .wrapr_private_var_env)
            nms <- names(.wrapr_private_var_ei)
            .wrapr_private_var_ei <- as.character(.wrapr_private_var_ei)
            if(!is.null(.wrapr_private_var_ni)) {
              nms <- paste(.wrapr_private_var_ni, nms, sep = '.')
            }
            names(.wrapr_private_var_ei) <- nms
            return(.wrapr_private_var_ei)
          }
        }
        # other case: quote expression
        .wrapr_private_var_ei <- paste(deparse(.wrapr_private_var_ei), collapse = "\n")
        if(!is.null(.wrapr_private_var_ni)) {
          names(.wrapr_private_var_ei) <- .wrapr_private_var_ni
        }
        return(.wrapr_private_var_ei)
      }
      if(is.vector(.wrapr_private_var_ei) || is.list(.wrapr_private_var_ei)) {
        if(length(.wrapr_private_var_ei)<=0) {
          return(character(0))
        }
      }
      # base case, character vectors, list, and objects
      .wrapr_private_var_ei <- paste(as.character(.wrapr_private_var_ei), collapse = " ")
      if(!is.null(.wrapr_private_var_ni)) {
        names(.wrapr_private_var_ei) <- .wrapr_private_var_ni
      }
      return(.wrapr_private_var_ei)
    })
  do.call(c, .wrapr_private_var_res, envir = .wrapr_private_var_env)
}

