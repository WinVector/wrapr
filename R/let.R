


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

prepareAlias <- function(alias) {
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
      stop(paste('wrapr:let alias values must all be strings or names (',
                 ni,'is class:', paste(class(vi), collapse=', '), ')'))
    }
    if (length(vi) != 1) {
      stop('wrapr:let alias values must all be single strings (not arrays or null)')
    }
    if (nchar(vi) <= 0) {
      stop('wrapr:let alias values must not be empty string')
    }
    if (!isValidAndUnreservedName(vi)) {
        stop(paste('wrapr:let alias value not a valid name: "', vi, '"'))
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

#' Substitute text (note text can be a vector).
#'
#' @param alias mapping named list/vector to strings/names or general
#' @param strexpr character vector source text to be re-written
#' @param ... force later arguments to be bound by name.
#' @param debugPrint logical if TRUE print debugging information
#' @return parsed R expression with substitutions
#'
#'
#'
letprep_str <- function(alias, strexpr,
                    ...,
                    debugPrint= FALSE) {
  if(length(list(...))>0) {
    stop("wrapr::letprep_str unexpected arguments.")
  }
  alias <- prepareAlias(alias)
  if(!is.character(strexpr)) {
    stop("wrapr::letprep_str strexpr must be a character array")
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


#' Substitute language elements.
#'
#' @param alias mapping named list/vector to strings/names or general
#' @param lexpr language item
#' @param ... force later arguments to be bound by name.
#' @return R language element with substitutions
#'
#'
#'
letprep_lang <- function(alias, lexpr) {
  nexpr <- lexpr
  n <- length(nexpr)
  # left-hand sides of lists/calls are represented as keys
  nms <- names(nexpr)
  if(length(nms)>0) {
    for(i in seq_len(length(nms))) {
      ki <- as.character(nms[[i]])
      if(length(ki)>0) {
        ri <- alias[[ki]]
        if((length(ri)>0)&&(ri!=ki)) {
          nms[[i]] <- ri
        }
      }
    }
    names(nexpr) <- nms
  }
  # try some easy cases first
  if(is.character(nexpr)) {
    return(nexpr)
  }
  # this is the main re-mapper
  if(is.symbol(nexpr)) { # same as is.name()
    # symbol is not subsettable, so length==1
    #  as.name('x')[[1]]
    #  # Error in as.name("x")[[1]] : object of type 'symbol' is not subsettable
    # and can't have names
    #   names(as.name("x")) <- 'a'
    #   ## Error in names(as.name("x")) <- "a" :
    #   ## target of assignment expands to non-language object
    ki <- as.character(nexpr)
    ri <- alias[[ki]]
    if((length(ri)>0)&&(ri!=ki)) {
      return(as.name(ri))
    }
    return(nexpr)
  }
  # just in case (establishes an invarient of n>=1)
  if(n<=0) {
    return(nexpr)
  }
  # special cases
  if(is.call(nexpr)) {
    callName <- as.character(nexpr[[1]])
    if(length(callName)==1) {
      # get into special cases,
      #  detect them very strictly and return out of them
      if((callName=='$') && (n==3)) {
        # special case a$"b"
        # let(c(x='y'), d$"x", eval=FALSE)
        # know length should be 3 from:
        #  do.call('$',list(data.frame(x=1:3),'x','z'))
        #  # Error in list(x = 1:3)$x : 3 arguments passed to '$' which requires 2
        # know the 3rd argument can be treated as a name from:
        #  data.frame(x=1:3)$1
        #  # Error: unexpected numeric constant in "data.frame(x=1:3)$1"
        nexpr[[2]] <- letprep_lang(alias, nexpr[[2]])
        nexpr[[3]] <- letprep_lang(alias, as.name(nexpr[[3]]))
        return(nexpr)
      }
    }
  }
  if(is.expression(nexpr) || is.call(nexpr)) {
    # from help(is.expression):
    #   "As an object of mode "expression" is a list"
    # as x[[1]] isn't x for lists  (which is not true for vectors as 1[[1]]  is 1)
    # we know we can try to recurse without an obvious infinite loop
    for(i in seq_len(n)) {
      nexpr[[i]] <- letprep_lang(alias, nexpr[[i]])
    }
    return(nexpr)
  }
  # should be done as is.language() is supposed to imply one of:
  #   is.name(), is.call(), or is.expr()
  # the recurse, assuming language objects are list-like in that
  #  x[[1]] isn't x (which is not true for vectors as 1[[1]]  is 1).
  # was recursing on is.language(), now do it on n>1
  if(n>1) {
    for(i in seq_len(n)) {
      nexpr[[i]] <- letprep_lang(alias, nexpr[[i]])
    }
    return(nexpr)
  }
  return(nexpr)
}



#' Execute expr with name substitutions specified in alias.
#'
#' \code{let} implements a mapping from desired names (names used directly in the expr code) to names used in the data.
#' Mnemonic: "expr code symbols are on the left, external data and function argument names are on the right."
#'
#' Please see the \code{wrapr} \code{vignette} for some discussion of let and crossing function call boundaries: \code{vignette('wrapr','wrapr')}.
#' Transformation is performed by substitution, so please be wary of name collisions or aliasing.
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
#' that is the more properly left to \code{R}'s environment systems.
#' \code{let} is intended to transform
#' "tame" variable and column names to "tame" variable and column names.  Substitution
#' outcomes that are not valid simple \code{R} variable names (produced with out use of
#' back-ticks) are forbidden.  It is suggested that substitution targets be written
#' \code{ALL_CAPS} style to make them stand out.
#'
#'
#' @param alias mapping from free names in expr to target names to use.
#' @param expr block to prepare for execution.
#' @param ... force later arguments to be bound by name.
#' @param subsMethod character substitution method, one of  c('langsubs', 'stringsubs', 'subsubs').
#' @param eval logical if TRUE execute the re-mapped expression (else return it).
#' @param debugPrint logical if TRUE print debugging information when in stringsubs mode.
#' @return result of expr executed in calling environment (or expression if eval==FALSE)
#'
#' @examples
#'
#' d <- data.frame(Sepal_Length=c(5.8,5.7),
#'                 Sepal_Width=c(4.0,4.4),
#'                 Species='setosa',
#'                 rank=c(1,2))
#'
#' RANKCOLUMN <- NULL # optional, make sure macro target does not look like unbound variable.
#' GROUPCOLUMN <- NULL # optional, make sure macro target does not look like unbound variable.
#' mapping = c(RANKCOLUMN= 'rank', GROUPCOLUMN= 'Species')
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
#' # The following 3 examples show the different consequences of the different substitutions methods:
#' y <- 7
#'
#' # In langsubs mode let replaces left/right sides, but not strings:
#' let(c('X'='y'), list(X=X, str='X'),
#'     subsMethod = 'langsubs', debugPrint = TRUE)
#' # list(y=7, str="X")
#' let(c('X'='y'), X <- list(X=X, str='X'),
#'     eval=FALSE, subsMethod= 'langsubs')
#' # y <- list(y = y, str = "X")
#'
#' # In string substitution mode let can replace string contents and left/right sides:
#' let(c('X'='y'), list(X=X, str='X'),
#'     subsMethod = 'stringsubs', debugPrint = TRUE)
#' # list(y=7, str="y")
#' let(c('X'='y'), X <- list(X=X, str='X'),
#'     eval=FALSE, subsMethod= 'stringsubs')
#' # expression(y <- list(y = y, str = "y"))
#'
#' # In subsubs mode strings and left-hand-side of function arguments are not
#' #  re-mapped (though left hand side of assignment is):
#' let(c('X'='y'), list(X=X, str='X'),
#'     subsMethod= 'subsubs', debugPrint = TRUE)
#' # list(X=7, str='X')
#' let(c('X'='y'), X <- list(X=X, str='X'),
#'     eval=FALSE, subsMethod= 'subsubs')
#' # y <- list(X = y, str = "X")
#'
#' @export
let <- function(alias, expr,
                ...,
                subsMethod= 'langsubs',
                eval= TRUE,
                debugPrint= FALSE) {
  exprQ <- substitute(expr)  # do this early before things enter local environment
  if(length(list(...))>0) {
    stop("wrapr::let unexpected arguments")
  }
  allowedMethods <- c('langsubs', 'stringsubs', 'subsubs')
  if((!is.character(subsMethod)) ||
     (length(subsMethod)!=1) ||
     (!(subsMethod %in% allowedMethods))) {
    stop(paste("wrapr::let subsMethod must be one of:",
               paste(allowedMethods, collapse = ', ')))
  }
  exprS <- NULL
  if(subsMethod=='langsubs') {
    # recursive language implementation.
    # only replace matching symbols.
    exprS <- letprep_lang(prepareAlias(alias),
                          exprQ)
    if(debugPrint) {
      print(exprS)
    }
  } else if(subsMethod=='subsubs') {
    # substitute based solution, does not bind left-hand sides
    aliasN <- lapply(prepareAlias(alias), as.name)
    exprS <- do.call(substitute, list(exprQ, aliasN))
    if(debugPrint) {
      print(exprS)
    }
  } else if(subsMethod=='stringsubs') {
    # string substitution based implementation.
    # Similar to \code{gtools::strmacro} by Gregory R. Warnes.
    exprS <- letprep_str(alias, deparse(exprQ),
                     debugPrint=debugPrint)
  } else {
    stop(paste("wrapr::let unexpected subsMethod '", subsMethod, "'"))
  }
  if(!eval) {
    return(exprS)
  }
  # try to execute expression in parent environment
  rm(list=setdiff(ls(),'exprS'))
  eval(exprS,
       envir=parent.frame(),
       enclos=parent.frame())
}



