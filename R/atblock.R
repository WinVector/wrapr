
#' Evaluate an expression with \code{@name} substitution (deprecated).
#'
#'
#' The expression represented by the text of the \code{.}-argument is evaluated in
#' the current environment will all "\code{@name}" forms replaced by the value of name.
#' This allows variables to carry the names of other variables into what would
#' be a non-standard evaluation situation.  This adaption allows some parametric
#' or standard-evaluation effects in such circumstances.
#'
#' Note: this method uses string substitution and is willing to substitute in arbitrary content,
#' please prefer using \code{\link{let}} where applicable.
#'
#'
#' Notation idea: Jonathan Carroll \url{https://jcarroll.com.au} \url{https://twitter.com/carroll_jono/status/842142292253196290}
#' Similar to: \url{https://dev.mysql.com/doc/refman/5.7/en/user-variables.html}.
#'
#' @seealso \code{\link{let}}
#'
#' @param . character text of expression or block to evaluate
#'
#' @export
#'
ateval <- function(.) {
  .Deprecated(new = "let", old = "ateval")
  exprtext <- paste0(' ', ., ' ')
  # find @symbols
  pattern <- '[^\\w._]@[[:alpha:]._][\\w._]*\\b'
  matches <- gregexpr(pattern, exprtext, perl=TRUE)
  starts <- as.numeric(matches[[1]])
  lens <- attr(matches[[1]],'match.length')
  syms <- vapply(seq_len(length(starts)),
                 function(i) {
                   si <- starts[[i]]
                   li <- lens[[i]]
                   v <- ''
                   if((si>=1)&&(li>=1)&&(si+li-1>=si+2)) {
                     v <- substr(exprtext, si+2, si+li-1)
                   }
                   v
                 }, character(1))
  syms <- Filter(function(symi) { nchar(symi)>0 }, syms)
  syms <- sort(unique(syms))
  # substitute for new names
  pf <- parent.frame()
  exprtext2 <- exprtext
  for(symi in syms) {
    pati <- paste0('@', symi, '\\b')
    vali <- get(symi, envir=pf)
    exprtext2 <- gsub(pati, vali, exprtext2, perl=TRUE)
  }
  eval(parse(text=exprtext2),
       envir=pf,
       enclos=pf)
}

#' Evaluate an expression with \code{(!!name)} and \code{:=} to \code{=} substitution (deprecated).
#'
#' Notation idea: \url{https://github.com/hadley/dplyr/commit/8f03f835185370626a566e95d268623b20189e07}.
#' Note: "\code{!!}" is not a no-op, but is a sufficiently uncommon expression one can use it.
#'
#' @seealso \code{\link{let}}
#'
#' @param ... expression or block to evaluate
#'
#' @export
#'
beval <- function(...) {
  .Deprecated(new = "let", old = "beval")
  exprtext0 <- deparse(substitute(alist(...)))
  exprtext <- gsub("^alist\\(","", exprtext0)
  exprtext <- gsub("\\)$","", exprtext)
  # find (!!symbols)
  pattern <- '\\(!\\(![[:alpha:]._][\\w._]*\\)\\)'
  matches <- gregexpr(pattern, exprtext, perl=TRUE)
  starts <- as.numeric(matches[[1]])
  lens <- attr(matches[[1]],'match.length')
  syms <- vapply(seq_len(length(starts)),
                 function(i) {
                   si <- starts[[i]]
                   li <- lens[[i]]
                   v <- ''
                   if((si>=1)&&(li>=1)&&(si+li-3>=si+4)) {
                     v <- substr(exprtext, si+4, si+li-3)
                   }
                   v
                 }, character(1))
  syms <- Filter(function(symi) { nchar(symi)>0 }, syms)
  syms <- sort(unique(syms))
  pf <- parent.frame()
  exprtext2 <- exprtext
  # subtitute := for = (work as assignment in statements:
  # varName <- 'x'
  # beval((!!varName) := 7)
  # and argument binding in dplyr verbs) such as:
  # http://www.win-vector.com/blog/2017/04/programming-over-r/
  # beval(d %>% mutate((!!rname) := is.na((!!cname))))
  # note string it is actually applied to is:
  #   beval(`:=`((!(!varName)), 7))
  # and the argument name does not get passed to mutate successfully
  exprtext2 <- gsub(':=', '=', exprtext2, fixed=TRUE)
  # substitute for new names
  for(symi in syms) {
    pati <- paste0('(!(!', symi, '))')
    vali <- get(symi, envir=pf)
    exprtext2 <- gsub(pati, vali, exprtext2, fixed=TRUE)
  }
  eval(parse(text=exprtext2),
       envir=pf,
       enclos=pf)
}


#' Execute expr with general substitutions specified in alias (deprecated).
#'
#' Note: this method uses string substitution and is willing to substitute in arbitrary content,
#' please prefer using \code{\link{let}} where applicable.
#'
#' @seealso \code{\link{let}}
#'
#'
#' @param alias mapping from free names in expr to target names to use.
#' @param expr block to prepare for execution.
#' @param ... force later arguments to be bound by name.
#' @param eval logical if TRUE execute the re-mapped expression (else return it).
#' @param debugPrint logical if TRUE print debugging information when in stringsubs mode.
#' @return result of expr executed in calling environment (or expression if eval==FALSE)
#'
#' @export
seval <- function(alias, expr,
                  ...,
                  eval= TRUE,
                  debugPrint= FALSE) {
  .Deprecated(new = "let", old = "seval")
  exprQ <- deparse(substitute(expr))  # do this early before things enter local environment
  if(length(list(...))>0) {
    stop("wrapr::seval unexpected arguments")
  }
  body <- exprQ
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
  exprS <- parse(text= body)
  if(debugPrint) {
    print(exprS)
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



