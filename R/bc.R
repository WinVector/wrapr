
#' Parse argument as a vector of values allowing "blank separators".
#'
#' Accepts simple strings that do not include escapes. Separates data on whitespace, separating symbols.
#' Words are assumed to start with '.', '_', or \code{getOption('wrapr.bc.alphabet')} (defaults to \code{[A-Za-z]} equivalent).
#' Separating symbols are whitespace and \code{getOption('wrapr.bc.seps')} (defaults to \code{,|}).
#' Suggested by Emil Erik Pula Bellamy Begtrup-Bright \url{https://github.com/WinVector/wrapr/issues/12}.
#'
#' @param s string to parse
#' @return vector of values
#'
#' @examples
#'
#' bc('1 2 "c", d') # returns c("1", "2", "c", "d")
#' bc('1 2 3') # returns c(1, 2, 3)
#' bc('1 2 "3"') # returns c("1", "2", "3")
#' bc('1,2|3.4') # returns c(1, 2, 3.4)
#'
#' @export
#'
bc <- function(s) {
  if(!is.character(s)) {
    stop("wrapr::bc s must be character")
  }
  if(length(s)!=1) {
    stop("wrapr::bc s must be length 1")
  }
  sep_symbols <- getOption('wrapr.bc.seps')
  single_quote_str <- "('([^']*)')"
  double_quote_str <- '("([^"]*)")'
  number_l <- '([+-]?(\\d)+[.]?(\\d)*([eE][+-]?(\\d)+)?)'
  number_r <- '([+-]?(\\d)*[.]?(\\d)+([eE][+-]?(\\d)+)?)'
  word <- paste0('([._', getOption('wrapr.bc.alphabet') ,']([._', getOption('wrapr.bc.alphabet') ,']|\\d)*)')
  space <- '(\\s+)'
  sep <- paste0('((\\s|[', sep_symbols, '])+)')
  pattern <- paste(
    single_quote_str,
    double_quote_str,
    number_l,
    number_r,
    word,
    space,
    sep,
    sep = '|')

  toks <- strsplit_capture(s,
                           split = pattern)
  is_waste <- paste0('^(\\s|[', sep_symbols, '])*$')
  indices <- grep(is_waste, toks, invert = TRUE)
  got <- as.character(toks[indices])
  num <- NULL
  tryCatch(
    num <- as.numeric(got),
    warning = function(w) {})
  if(!is.null(num)) {
    return(num)
  }
  single_quote_str_all <- "(^'([^']*)'$)"
  double_quote_str_all <- '(^"([^"]*)"$)'
  quoted <- paste(
    single_quote_str_all,
    double_quote_str_all,
    sep = '|')
  indices <- grep(quoted, got)
  for(i in indices) {
    ni <- nchar(got[[i]])
    got[[i]] <- substr(got[[i]], 2, ni - 1)
  }
  return(got)
}

