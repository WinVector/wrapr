


# checking for valid unreserved names
# this one is not vectorized
# also don't allow dot to be used here as remapping that is problem in magrittr pipelines
#  this is essentially treating "." as reserved (which is more compatible with magrittr)
# from: http://stackoverflow.com/questions/8396577/check-if-character-value-is-a-valid-r-object-name
isValidAndUnreservedName <- function(string) {
  (is.character(string)) &&
    (length(string)==1) &&
    (string!='.') &&
    (make.names(string,unique = FALSE, allow_ = TRUE) == string)
}

#' Restrict an alias mapping list to things that look like name assignments
#'
#' @param alias mapping list
#' @param restrictToAllCaps logical, if true only use all-capitalized keys
#' @return string to string mapping
#'
#' @examples
#'
#' alias <- list(region= 'east', str= "'seven'")
#' aliasR <- restrictToNameAssignments(alias)
#' print(aliasR)
#'
#'
#' @export
#'
restrictToNameAssignments <- function(alias, restrictToAllCaps=TRUE) {
  # make sure alias is a list (not a named vector)
  alias <- as.list(alias)
  usableEntries <- vapply(names(alias),
                          function(ai) {
                            vi <- alias[[ai]]
                            isValidAndUnreservedName(ai) && isValidAndUnreservedName(vi) &&
                            ( (!restrictToAllCaps) || (toupper(ai)==ai))
                          },
                          logical(1))
  # return sublist
  alias[usableEntries]
}

letprep <- function(alias, strexpr) {
  # make sure alias is a list (not a named vector)
  alias <- as.list(alias)
  # confirm alias is mapping strings to strings
  if (length(unique(names(alias))) != length(names(alias))) {
    stop('wrapr::let alias keys must be unique')
  }
  if ('.' %in% c(names(alias),as.character(alias))) {
    stop("wrapr::let can not map to/from '.'")
  }
  for (ni in names(alias)) {
    if (is.null(ni)) {
      stop('wrapr:let alias keys must not be null')
    }
    if (!is.character(ni)) {
      stop('wrapr:let alias keys must all be strings')
    }
    if (length(ni) != 1) {
      stop('wrapr:let alias keys must all be strings')
    }
    if (nchar(ni) <= 0) {
      stop('wrapr:let alias keys must be empty string')
    }
    if (!isValidAndUnreservedName(ni)) {
      stop(paste('wrapr:let alias key not a valid name: "', ni, '"'))
    }
    vi <- alias[[ni]]
    if (is.null(vi)) {
      stop('wrapr:let alias values must not be null')
    }
    if (!is.character(vi)) {
      stop('wrapr:let alias values must all be strings')
    }
    if (length(vi) != 1) {
      stop('wrapr:let alias values must all be single strings (not arrays)')
    }
    if (nchar(vi) <= 0) {
      stop('wrapr:let alias values must not be empty string')
    }
    if (!isValidAndUnreservedName(vi)) {
      stop(paste('wrapr:let alias value not a valid name: "', vi, '"'))
    }
    if(vi!=ni) {
      if(vi %in% names(alias)) {
        stop("wrapr::let except for identity assignments keys and destinations must be disjoint")
      }
    }
  }
  # re-write the parse tree and prepare for execution
  body <- strexpr
  for (ni in names(alias)) {
    value <- alias[[ni]]
    if(ni!=value) {
      pattern <- paste0("\\b", ni, "\\b")
      body <- gsub(pattern, value, body)
    }
  }
  parse(text = body)
}

#' Execute expr with name substitutions specified in alias.
#'
#' \code{let} implements a mapping from desired names (names used directly in the expr code) to names used in the data.
#' Mnemonic: "expr code symbols are on the left, external data and function argument names are on the right."
#'
#'
#'
#' Inspired by \code{gtools::strmacro} by Gregory R. Warnes.
#' Please see the \code{wrapr} \code{vignette} for some discussion of let and crossing function call boundaries: \code{vignette('wrapr','wrapr')}.
#' Transformation is performed by substitution on the expression parse tree, so be wary of name collisions or aliasing.
#'
#' Something like \code{let} is only useful to get control of a function that is parameterized
#' (in the sense it take column names) but non-standard (in that it takes column names from
#' non-standard evaluation argument name capture, and not as simple variables or parameters).  So  \code{wrapr:let} is not
#' useful for non-parameterized functions (functions that work only over values such as \code{base::sum}),
#' and not useful for functions take parameters in straightforward way (such as \code{base::merge}'s "\code{by}" argument).
#' \code{dplyr::mutate} is an example where
#' we can use a \code{let} helper.   \code{dplyr::mutate} is
#' parameterized (in the sense it can work over user supplied columns and expressions), but column names are captured through non-standard evaluation
#' (and it rapidly becomes unwieldy to use complex formulas with the standard evaluation equivalent \code{dplyr::mutate_}).
#' \code{alias} can not include the symbol "\code{.}". Except for identity assignments keys and destinations must be disjoint.
#'
#'
#' @param alias mapping from free names in expr to target names to use.
#' @param expr block to prepare for execution
#' @return result of expr executed in calling environment
#'
#' @examples
#'
#' d <- data.frame(Sepal_Length=c(5.8,5.7),
#'                 Sepal_Width=c(4.0,4.4),
#'                 Species='setosa',
#'                 rank=c(1,2))
#'
#' mapping = list(RankColumn='rank',GroupColumn='Species')
#' let(alias=mapping,
#'     expr={
#'        # Notice code here can be written in terms of known or concrete
#'        # names "RankColumn" and "GroupColumn", but executes as if we
#'        # had written mapping specified columns "rank" and "Species".
#'
#'        # restart ranks at zero.
#'        dres <- d
#'        dres$RankColumn <- dres$RankColumn - 1 # notice using $ not [[]]
#'
#'        # confirm set of groups.
#'        groups <- unique(d$GroupColumn)
#'     })
#' print(groups)
#' print(length(groups))
#' print(dres)
#'
#' # let works by string substitution aligning on word boundaries,
#' # so it does (unfortunately) also re-write strings.
#' let(list(x='y'),'x')
#'
#' @export
let <- function(alias, expr) {
  # capture expr
  strexpr <- deparse(substitute(expr))
  `_reply_reserved_name` <- letprep(alias,strexpr)
  rm(list=setdiff(ls(all.names=TRUE),list('_reply_reserved_name')))
  # try to execute expression in parent environment
  eval(`_reply_reserved_name`, envir=parent.frame(), enclos=parent.frame())
}
