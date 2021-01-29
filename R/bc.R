
#' Parse argument as a vector of values allowing "blank separators".
#'
#' Accepts simple string that do not include escapes. Separates data on whitespace, comma, and pipe.
#' Pattern matching adapted for US English.
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
  single_quote_str <- "('([^']*)')"
  double_quote_str <- '("([^"]*)")'
  number_l <- '([+-]?(\\d)+[.]?(\\d)*([e|E][+-]?(\\d)+)?)'
  number_r <- '([+-]?(\\d)*[.]?(\\d)+([e|E][+-]?(\\d)+)?)'
  word <- '([._A-Za-z]\\w*)'
  space <- '(\\s+)'
  sep <- '((\\s|[,|])+)'
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
  is_waste <- '^(\\s|[,|])*$'
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

