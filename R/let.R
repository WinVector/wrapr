


# checking for valid unreserved names
# this one is not vectorized
# also don't allow dot to be used here as remapping that is problem in magrittr pipelines
#  this is essentially treating "." as reserved (which is more compatible with magrittr)
# from: http://stackoverflow.com/questions/8396577/check-if-character-value-is-a-valid-r-object-name
isValidAndUnreservedName <- function(string) {
  if(is.null(string)) {
    return(FALSE)
  }
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
restrictToNameAssignments <- function(alias, restrictToAllCaps= FALSE) {
  # make sure alias is a list (not a named vector)
  alias <- as.list(alias)
  usableEntries <- vapply(names(alias),
                          function(ai) {
                            vi <- alias[[ai]]
                            if(is.name(vi)) {
                              vi <- as.character(vi)
                            }
                            isValidAndUnreservedName(ai) && isValidAndUnreservedName(vi) &&
                            ( (!restrictToAllCaps) || (toupper(ai)==ai))
                          },
                          logical(1))
  # return sublist
  alias[usableEntries]
}

prepareAlias <- function(alias, strict) {
  # make sure alias is a list (not a named vector)
  alias <- as.list(alias)
  # skip any NULL slots
  nulls <- vapply(names(alias), is.null, logical(1)) |
    vapply(alias, is.null, logical(1))
  alias <- alias[!nulls]
  # confirm alias is mapping strings to strings
  if (length(unique(names(alias))) != length(names(alias))) {
    stop('wrapr::prepareAlias alias keys must be unique')
  }
  if ('.' %in% c(names(alias),as.character(alias))) {
    stop("wrapr::prepareAlias can not map to/from '.'")
  }
  for (ni in names(alias)) {
    if (is.null(ni)) {
      stop('wrapr:let alias keys must not be null')
    }
    if (!is.character(ni)) {
      stop('wrapr:let alias keys must all be strings')
    }
    if (length(ni) != 1) {
      stop('wrapr:let alias keys must all be scalars')
    }
    if (nchar(ni) <= 0) {
      stop('wrapr:let alias keys must be non-empty string')
    }
    if (!isValidAndUnreservedName(ni)) {
      stop(paste('wrapr:let alias key not a valid name: "', ni, '"'))
    }
    vi <- alias[[ni]]
    if (is.null(vi)) {
      stop('wrapr:let alias values must not be null')
    }
    if (is.name(vi)) {
      vi <- as.character(vi)
    }
    if (!is.character(vi)) {
      stop('wrapr:let alias values must all be strings or names')
    }
    if (length(vi) != 1) {
      stop('wrapr:let alias values must all be single strings (not arrays or null)')
    }
    if (nchar(vi) <= 0) {
      stop('wrapr:let alias values must not be empty string')
    }
    if(strict) {
      if (!isValidAndUnreservedName(vi)) {
        stop(paste('wrapr:let alias value not a valid name: "', vi, '"'))
      }
    }
    if(vi!=ni) {
      if(vi %in% names(alias)) {
        stop("wrapr::prepareAlias except for identity assignments keys and destinations must be disjoint")
      }
    }
  }
  alias <- lapply(alias, as.character)
  alias
}

#' Substitute text.
#'
#' @param alias mapping named list/vector to strings/names or general
#' @param strexpr character vector source text to be re-written
#' @param ... force later arguments to be bound by name.
#' @param strict logical if TRUE only map to non-reserved names
#' @param debugPrint logical if TRUE print debugging information
#' @return parsed R expression
#'
#' @examples
#'
#'
#' letprep(alias= list(RANKCOLUMN= 'rank', GROUPCOLUMN= 'Species'),
#'     strexpr= '{
#'        # Notice code here can be written in terms of known or concrete
#'        # names "RANKCOLUMN" and "GROUPCOLUMN", but executes as if we
#'        # had written mapping specified columns "rank" and "Species".
#'
#'        # restart ranks at zero.
#'        dres <- d
#'        dres$RANKCOLUMN <- dres$RANKCOLUMN - 1 # notice using $ not [[]]
#'
#'        # confirm set of groups.
#'        groups <- unique(d$GROUPCOLUMN)
#'     }',
#'     strict= TRUE)
#'
#'
#' @export
#'
#'
letprep <- function(alias, strexpr,
                    ...,
                    strict= TRUE,
                    debugPrint= FALSE) {
  if(length(list(...))>0) {
    stop("wrapr::letprep unexpected arguments.")
  }
  alias <- prepareAlias(alias, strict)
  if(!is.character(strexpr)) {
    stop("wrapr::letprep strexpr must be length 1 character array")
  }
  # re-write the parse tree and prepare for execution
  body <- strexpr
  for (ni in names(alias)) {
    value <- alias[[ni]]
    if(!is.null(value)) {
      value <- as.character(value)
      if(ni!=value) {
        pattern <- paste0("\\b", ni, "\\b")
        body <- gsub(pattern, value, body)
      }
    }
  }
  if(debugPrint) {
    print(body)
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
#' The intent from is from the user perspective to have (if
#' \code{a <- 1; b <- 2}):
#' \code{let(c(z = 'a'), z+b)} to behave a lot like
#' \code{eval(substitute(z+b, c(z=quote(a))))}.
#'
#' \code{let} deliberately checks that it is mapping only to legal \code{R} names;
#' this is to discourage the use of \code{let} to make names to arbitrary values, as
#' that is the more properly left to \code{R}'s enviroment systems.
#'
#'
#' @param alias mapping from free names in expr to target names to use.
#' @param expr block to prepare for execution.
#' @param ... force later arguments to be bound by name.
#' @param strict logical is TRUE restrict map values to non-reserved non-dot names.
#' @param debugPrint logical if TRUE print debugging information
#' @return result of expr executed in calling environment
#'
#' @examples
#'
#' d <- data.frame(Sepal_Length=c(5.8,5.7),
#'                 Sepal_Width=c(4.0,4.4),
#'                 Species='setosa',
#'                 rank=c(1,2))
#'
#' RANKCOLUMN <- NULL # optional, make sure marco target does not look like unbound variable.
#' GROUPCOLUMN <- NULL # optional, make sure marco target does not look like unbound variable.
#' mapping = list(RANKCOLUMN= 'rank', GROUPCOLUMN= 'Species')
#' let(alias=mapping,
#'     expr={
#'        # Notice code here can be written in terms of known or concrete
#'        # names "RANKCOLUMN" and "GROUPCOLUMN", but executes as if we
#'        # had written mapping specified columns "rank" and "Species".
#'
#'        # restart ranks at zero.
#'        dres <- d
#'        dres$RANKCOLUMN <- dres$RANKCOLUMN - 1 # notice using $ not [[]]
#'
#'        # confirm set of groups.
#'        groups <- unique(d$GROUPCOLUMN)
#'     })
#' print(groups)
#' print(length(groups))
#' print(dres)
#'
#' # let works by string substitution aligning on word boundaries,
#' # so it does (unfortunately) also re-write strings.
#' let(list(x='y'), 'x')
#'
#' # let can also substitute arbitrary expressions if strict=FALSE
#' let(list(e='1+3'), e, strict= FALSE)
#'
#' @export
let <- function(alias, expr,
                ...,
                strict= TRUE,
                debugPrint= FALSE) {
  if(length(list(...))>0) {
    stop("wrapr::let unexpected arguments")
  }
  # try to execute expression in parent environment
  # string substitution based implementation
  exprS <- letprep(alias, deparse(substitute(expr)),
                   strict=strict,
                   debugPrint=debugPrint)
  eval(exprS,
       envir=parent.frame(),
       enclos=parent.frame())
}


#' Conditionally echo and always execute expression (equivilant to let() with no alias).
#'
#' @param expr block to prepare for execution.
#' @param ... force later arguments to be bound by name.
#' @param debugPrint logical if TRUE print debugging information
#' @return result of expr executed in calling environment
#'
#' @examples
#'
#' echoAndExecute(1+1, debugPrint= FALSE)
#' echoAndExecute(1+1, debugPrint= TRUE)
#'
#' @export
echoAndExecute <- function(expr,
                           ...,
                           debugPrint= FALSE) {
  if(length(list(...))>0) {
    stop("wrapr::echoAndExecute unexpected arguments")
  }
  # try to execute expression in parent environment
  # string substitution based implementation
  exprS <- letprep(NULL, deparse(substitute(expr)),
                   strict=TRUE,
                   debugPrint=debugPrint)
  eval(exprS,
       envir=parent.frame(),
       enclos=parent.frame())
}

# #
# # a <- 1
# # b <- 2
# # # Given:
# # let(c(z = 'a'), z+b)
# # # Behaves a lot like:
# # eval(substitute(z+b, c(z=quote(a))))
# # # You would think the following below would be an easy
# # # realization of "let".
# # wrapr:::letSub(c(z = quote(a)), z+b)
# # # This fails because it is hard to control the when/were
# # # of both the substitute and eval at the same time.
# # # Likely some form of enquote, list2env and so on
# # # can get this to work, but it doesn't seem
# # # attractive at this time.
# #
# #
# letSub <-  function(alias, expr) {
#   # quote based implementation, not working
#   # pryr::subs() works about the same
#   # pryr::substitute_q() might fix
#   eval(substitute(expr, alias),
#        envir=parent.frame(),
#        enclos=parent.frame())
# }
