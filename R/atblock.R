
#' Evaluate an expression with @-value substitution
#'
#'
#' The expression represented by the text of the .-argument is evaluated in
#' the current environment will all "@name" forms replaced by the value of name.
#' This allows variables to carry the names of other variables into what would
#' be a non-standard evaluation situation.  This adaption allows some parametric
#' or standard-evaluation effects in such circumstances.
#'
#'
#' Notation idea: Jonathan Carroll \url{https://jcarroll.com.au} \url{https://twitter.com/carroll_jono/status/842142292253196290}
#' Similar to: \url{https://dev.mysql.com/doc/refman/5.7/en/user-variables.html}
#'
#' @param . character text of expression or block to evaluate
#'
#' @examples
#'
#'
#' c1 <- 1:5
#' c2 <- 3:7
#' col1 <- 'c1'
#' col2 <- 'c2'
#' new_col_name <- 'res'
#' ateval(
#'   '@new_col_name = @col1 + @col2'
#' )
#' print(res)
#'
#'
#' @export
#'
ateval <- function(.) {
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
                   if((si>=1)&&(li>=1)) {
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
    pati <- paste0('@', symi)
    # vali <- eval(parse(text=symi),
    #              envir=pf,
    #              enclos=pf)
    vali <- get(symi, envir=pf)
    if(is.name(vali)) {
      vali <- as.character(vali)
    }
    if(!is.character(vali)) {
      stop(paste("wrapr::atblock", pati, "must be a name or string"))
    }
    exprtext2 <- gsub(pati, vali, exprtext2, fixed=TRUE)
  }
  eval(parse(text=exprtext2),
       envir=pf,
       enclos=pf)
}
