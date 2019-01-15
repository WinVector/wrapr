

#' Split a string, keeping separator regions
#'
#' @param x character string to split (length 1 vector)
#' @param split split pattern
#' @param ... force later arguments to bind by name
#' @param ignore.case passed to gregexpr
#' @param perl passed to gregexpr
#' @param fixed passed to gregexpr
#' @param useBytes passed to gregexpr
#' @return list of string segments annotated with is_sep.
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
    return(x)
  }
  intervals <- sort(unique(c(1, nchar(x)+1, idxs, idxs+lens)))
  pieces <- lapply(
    seq_len(length(intervals)-1),
    function(i) {
      is_match <- intervals[[i]] %in% idxs
      pi <- substr(x, intervals[[i]], intervals[[i+1]]-1)
      attr(pi, "is_sep") <- is_match
      pi
    })
  pieces
}

#' Dot substitution.
#'
#' String interpolation using \code{bquote}-stype .() notation.  See also
#' \code{glue::glue()}.  Pure R, no C/C++ code called.
#'
#' @param str charater strings to be substituted into
#' @param ... force later arguments to bind by name
#' @param envir environemnt to look for values
#' @param enclos enclosing evaluation environment
#' @param match_pattern regexp to find substitution targets.
#' @param removal_patterns regexps to remove markers from substitution targets.
#' @return modified strings
#'
#' @examples
#'
#' x <- 7
#' sinterp(c("x is .(x), x+1 is .(x+1)", ".(x) is odd is .(x%%2 == 1)"))
#'
#' # Because matching is done by a regular expression we
#' # can not use parenthesis inside the interpolation region.
#' # One can work around that by pre-arranging values to be
#' # in variables or by using {} for expressions and
#' # pipe for single argument function evaluation.
#' # This is obviously awkward, but does work
#' # (assuming the wrapr package is attached):
#' sinterp("sin(x*(x+1)) is .({x*{x+1}} %.>% sin)")
#'
#' @export
#'
sinterp <- function(str,
                    ...,
                    envir = parent.frame(),
                    enclos = parent.frame(),
                    match_pattern = "\\.\\([^()]+\\)",
                    removal_patterns = c("^\\.\\(", "\\)$")) {
  force(envir)
  force(enclos)
  wrapr::stop_if_dot_args(substitute(list(...)), "wrapr::sinterp")
  nstr <- length(str)
  res <- character(nstr)
  for(i in seq_len(nstr)) {
    si <- str[[i]]
    pi <- strsplit_capture(si, match_pattern)
    npi <- length(pi)
    xlated <- character(npi)
    for(j in seq_len(npi)) {
      pij <- pi[[j]]
      if(!isTRUE(attr(pij, "is_sep", exact = TRUE))) {
        xlated[[j]] <- as.character(pij) # strip attributes.
      } else {
        expr <- as.character(pij) # strip attributes.
        for(rp in removal_patterns) {
          expr <- as.character(gsub(rp, "", expr))
        }
        val <- eval(parse(text = expr), envir = envir, enclos = enclos)
        val <- paste(as.character(val), collapse = " ")
        xlated[[j]] <- val
      }
    }
    res[[i]] <- paste(xlated, collapse = "")
  }
  res
}

