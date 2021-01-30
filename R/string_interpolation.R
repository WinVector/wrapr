

#' Split a string, keeping separator regions
#'
#' @param x character string to split (length 1 vector)
#' @param split split pattern
#' @param ... force later arguments to bind by name
#' @param ignore.case passed to \code{gregexpr}
#' @param perl passed to \code{gregexpr}
#' @param fixed passed to \code{gregexpr}
#' @param useBytes passed to \code{gregexpr}
#' @return list of string segments annotated with is_sep.
#'
#' @seealso \code{\link{sinterp}}, \code{\link{si}}
#'
#' @examples
#'
#' strsplit_capture("x is .(x) and x+1 is .(x+1)", "\\.\\([^()]+\\)")
#'
#' @export
#'
strsplit_capture <- function(x, split,
                      ...,
                      ignore.case = FALSE,
                      fixed = FALSE,
                      perl = FALSE,
                      useBytes = FALSE) {
  if(!is.character(x)) {
    stop("wrapr::strsplit_capture x must be character")
  }
  if(length(x)!=1) {
    stop("wrapr::strsplit_capture x must be length 1")
  }
  wrapr::stop_if_dot_args(substitute(list(...)), "wrapr::strsplit_capture")
  matches <- gregexpr(split, x,
                      ignore.case = ignore.case,
                      perl = perl,
                      fixed = fixed,
                      useBytes = useBytes)[[1]]
  lens <- attr(matches, "match.length", exact = TRUE)
  idxs <- as.integer(matches)
  if((length(idxs)<1) || (idxs[[1]]<1)) {
    attr(x, "is_sep") <- FALSE
    return(list(x))
  }
  match_posns <- logical(nchar(x)+1)
  match_posns[idxs] <- TRUE
  intervals <- sort(unique(c(1, nchar(x)+1, idxs, idxs+lens)))
  pieces <- lapply(
    seq_len(length(intervals)-1),
    function(i) {
      is_match <- match_posns[[intervals[[i]]]]
      pi <- substr(x, intervals[[i]], intervals[[i+1]]-1)
      attr(pi, "is_sep") <- is_match
      pi
    })
  pieces
}

#' Dot substitution string interpolation.
#'
#' String interpolation using \code{bquote}-stype .() notation. Pure R, no C/C++ code called.
#'
#' See also
#' \url{https://CRAN.R-project.org/package=R.utils},
#' \url{https://CRAN.R-project.org/package=rprintf},
#' and \url{https://CRAN.R-project.org/package=glue}.
#'
#'
#' @param str charater string(s) to be substituted into
#' @param ... force later arguments to bind by name
#' @param envir environemnt to look for values
#' @param enclos enclosing evaluation environment
#' @param match_pattern regexp to find substitution targets.
#' @param removal_patterns regexps to remove markers from substitution targets.
#' @return modified strings
#'
#' @seealso \code{\link{strsplit_capture}}, \code{\link{si}}
#'
#' @examples
#'
#' x <- 7
#' sinterp("x is .(x), x+1 is .(x+1)\n.(x) is odd is .(x%%2 == 1)")
#'
#' # Because matching is done by a regular expression we
#' # can not use arbitrary depths of nested parenthesis inside
#' # the interpolation region.  The default regexp allows
#' # one level of nesting (and one can use {} in place
#' # of parens in many places).
#' sinterp("sin(x*(x+1)) is .(sin(x*{x+1}))")
#'
#' # We can also change the delimiters,
#' # in this case to !! through the first whitespace.
#' sinterp(c("x is !!x , x+1 is !!x+1 \n!!x  is odd is !!x%%2==1"),
#'         match_pattern = '!![^[:space:]]+[[:space:]]?',
#'         removal_patterns = c("^!!", "[[:space:]]?$"))
#'
#' @export
#'
sinterp <- function(str,
                    ...,
                    envir = parent.frame(),
                    enclos = parent.frame(),
                    match_pattern = "\\.\\((([^()]+)|(\\([^()]*\\)))+\\)",
                    removal_patterns = c("^\\.\\(", "\\)$")) {
  force(envir)
  if(!is.environment(envir)) {
    envir <- list2env(as.list(envir), parent = parent.frame())
  }
  force(enclos)
  if(!is.environment(enclos)) {
    enclos <- list2env(as.list(enclos), parent = parent.frame())
  }
  wrapr::stop_if_dot_args(substitute(list(...)), "wrapr::sinterp")
  if(!is.character(str)) {
    stop("wrapr::sinterp str must be of class character")
  }
  if(length(str) <= 0) {
    stop("wrapr::sinterp str must be of length at least 1")
  }
  orig_names <- names(str)
  res <- vapply(
    str,
    function(stri) {
      pi <- strsplit_capture(stri, match_pattern)
      npi <- length(pi)
      xlated <- list()
      for(j in seq_len(npi)) {
        pij <- pi[[j]]
        if(!isTRUE(attr(pij, "is_sep", exact = TRUE))) {
          xlated <- c(xlated, list(as.character(pij))) # strip attributes.
        } else {
          expr <- as.character(pij) # strip attributes.
          for(rp in removal_patterns) {
            expr <- as.character(gsub(rp, "", expr))
          }
          val <- eval(parse(text = expr), envir = envir, enclos = enclos)
          val <- deparse(val)
          xlated <- c(xlated, list(val))
        }
      }
      do.call(paste0, xlated)
    },
    character(1))
  if(length(orig_names) <= 0) {
    names(res) <- NULL
  }
  return(res)
}


#' Dot substitution string interpolation.
#'
#' String interpolation using \code{bquote}-stype .() notation. Pure R, no C/C++ code called.
#' \code{sinterp} and \code{si} are synonyms.
#'
#' See also
#' \url{https://CRAN.R-project.org/package=R.utils},
#' \url{https://CRAN.R-project.org/package=rprintf},
#' and \url{https://CRAN.R-project.org/package=glue}.
#'
#'
#' @param str charater string to be substituted into
#' @param ... force later arguments to bind by name
#' @param envir environemnt to look for values
#' @param enclos enclosing evaluation environment
#' @param match_pattern regexp to find substitution targets.
#' @param removal_patterns regexps to remove markers from substitution targets.
#' @return modified strings
#'
#' @seealso \code{\link{strsplit_capture}}, \code{\link{sinterp}}
#'
#' @examples
#'
#' x <- 7
#' si("x is .(x), x+1 is .(x+1)\n.(x) is odd is .(x%%2 == 1)")
#'
#' # Because matching is done by a regular expression we
#' # can not use arbitrary depths of nested parenthesis inside
#' # the interpolation region.  The default regexp allows
#' # one level of nesting (and one can use {} in place
#' # of parens in many places).
#' si("sin(x*(x+1)) is .(sin(x*{x+1}))")
#'
#' # We can also change the delimiters,
#' # in this case to !! through the first whitespace.
#' si(c("x is !!x , x+1 is !!x+1 \n!!x  is odd is !!x%%2==1"),
#'    match_pattern = '!![^[:space:]]+[[:space:]]?',
#'    removal_patterns = c("^!!", "[[:space:]]?$"))
#'
#'
#' @export
#'
si <- sinterp


#' Dot substitution string interpolation.
#'
#' String interpolation using \code{bquote}-stype .() notation. Pure R, no C/C++ code called.
#'
#' See also
#' \url{https://CRAN.R-project.org/package=R.utils},
#' \url{https://CRAN.R-project.org/package=rprintf},
#' and \url{https://CRAN.R-project.org/package=glue}.
#'
#'
#' @param str charater string to be substituted into
#' @param envir environemnt to look for values
#' @return modified strings
#'
#' @seealso \code{\link{strsplit_capture}}, \code{\link{si}}
#'
#' @examples
#'
#' "x is .(x)" %<s% list(x = 7)
#'
#'
#' @export
#'
`%<s%` <- function(str, envir) {
  force(envir)
  sinterp(str, envir = envir, enclos = envir)
}

#' Dot substitution string interpolation.
#'
#' String interpolation using \code{bquote}-stype .() notation. Pure R, no C/C++ code called.
#'
#' See also
#' \url{https://CRAN.R-project.org/package=R.utils},
#' \url{https://CRAN.R-project.org/package=rprintf},
#' and \url{https://CRAN.R-project.org/package=glue}.
#'
#'
#' @param envir environemnt to look for values
#' @param str charater string to be substituted into
#' @return modified strings
#'
#' @seealso \code{\link{strsplit_capture}}, \code{\link{si}}
#'
#' @examples
#'
#' list(x = 7) %s>% "x is .(x)"
#'
#'
#' @export
#'
`%s>%` <- function(envir, str) {
  force(envir)
  sinterp(str, envir = envir, enclos = envir)
}

