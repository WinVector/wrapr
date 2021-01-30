
#' Blank Concatenate. Parse argument as a vector of values allowing "blank separators".
#'
#' Separates string data on whitespace and separating symbols into an array.
#'
#' Can throw exception on lack of explicit value separators, example: \code{bc('"a""b"')} and  non-matching portions.
#' Whitespace is normalized to spaces.
#' Suggested by Emil Erik Pula Bellamy Begtrup-Bright \url{https://github.com/WinVector/wrapr/issues/12}.
#'
#' @param s string to parse
#' @param ... force later arguments to be set by name
#' @param sep_symbols characters to consider separators
#' @param strict logical, if TRUE throw exception on confusing input
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
bc <- function(
  s,
  ...,
  sep_symbols = ',|',
  strict = TRUE) {
  # check arguments
  if(!is.character(s)) {
    stop("wrapr::bc s must be character")
  }
  if(length(s)!=1) {
    stop("wrapr::bc s must be length 1")
  }
  wrapr::stop_if_dot_args(substitute(list(...)), "wrapr::bc")

  # replace all white-space with space
  s <- gsub('\\s', ' ', s)

  # tear up string
  single_quote_str <- "('((\\\\.)|[^'])*')"  # with escapes
  double_quote_str <- gsub("'", '"', single_quote_str, fixed = TRUE)
  number_l <- '([+-]?(\\d)+[.]?(\\d)*([eE][+-]?(\\d)+)?)'
  number_r <- '([+-]?(\\d)*[.]?(\\d)+([eE][+-]?(\\d)+)?)'  # can overlap with other number: 1.1
  hex <- '(0[xX][0-9a-fA-F]+)'
  symbol_regexp <- paste0('([^0-9 \\\'"', sep_symbols,'+-][^ \\\'"', sep_symbols,']*)')  # can overlap with number: .4
  sep_regexp <- paste0('([ ', sep_symbols, ']+)')
  pattern <- paste(
    single_quote_str,
    double_quote_str,
    number_l,
    number_r,
    hex,
    symbol_regexp,
    sep_regexp,
    sep = '|')
  toks <- strsplit_capture(s,
                           split = pattern)

  # special case length 0
  if(length(toks)<=0) {
    return(c())
  }
  # special case empty-return
  if((length(toks)==1) && (nchar(toks[[1]])<=0)) {
    return(c())
  }

  if(strict) {
    # insist all regions were recognized
    matched <- vapply(
      seq_len(length(toks)),
      function(i) { attr(toks[[i]], 'is_sep', exact = TRUE) },
      logical(1))
    if(!all(matched)) {
      min_index = which(!matched)[[1]]
      stop(paste0(
        "non-matched token: ",
        toks[min_index]))
    }
  }

  # limit down to non-separator regions
  is_waste <- paste0('^([ ', sep_symbols, ']*)$')
  indices <- grep(is_waste, toks, invert = TRUE)

  # special case length 0
  if(length(indices)<=0) {
    return(c())
  }

  if(strict) {
    # insist on non-consecutive value carrying indices
    if(length(intersect(indices, indices-1))) {
      min_index = min(intersect(indices, indices-1))
      stop(paste0(
        "missing explicit separator, not safe to return value: ",
        toks[min_index],
        toks[min_index + 1]))
    }
  }
  got <- as.character(toks[indices])  # also dump attributes

  # see if we can convert to logical
  logical <- NULL
  tryCatch(
    logical <- as.logical(got),  # currently as.logical does not warn
    warning = function(w) {},
    error = function(e) {})
  if((!is.null(logical)) && (all(!is.na(logical)))) {
    return(logical)
  }

  # see if we can convert to numeric
  num <- NULL
  tryCatch(
    num <- as.numeric(got),  # currently as.numeric does warn
    warning = function(w) {},
    error = function(e) {})
  if((!is.null(num)) && (all(!is.na(num)))) {
    return(num)
  }

  # remove extra quotes
  single_quote_str_all <- paste0('^', single_quote_str, '$')
  double_quote_str_all <-  paste0('^', double_quote_str, '$')
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

