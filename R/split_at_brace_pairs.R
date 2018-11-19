


#' Split strings at {}-pairs.
#'
#' @param s string or list of strings to split.
#' @return array or list of split strings.
#'
#' @examples
#'
#' split_at_brace_pairs("{x} + y + {z}")
#'
#' @export
#'
split_at_brace_pairs <- function(s) {
  if(length(s)<1) {
    return(s)
  }
  if(is.list(s)) {
    return(lapply(s, split_at_brace_pairs))
  }
  if(length(s)>1) {
    return(lapply(s, split_at_brace_pairs))
  }
  if(!is.character(s)) {
    return(s)
  }
  nc <- nchar(s)
  if(nc<1) {
    return(s)
  }
  lefts <- as.numeric(gregexpr("{", s, fixed = TRUE)[[1]])
  if(length(lefts)<=0) {
    return(s)
  }
  rights <- as.numeric(gregexpr("}", s, fixed = TRUE)[[1]])
  ng = length(lefts)
  # lefts and rights are supposed to be alternating, starting with left
  if(length(rights)!=ng) {
    return(s)
  }
  if(!isTRUE(all(lefts<rights))) {
    return(s)
  }
  if(!isTRUE(all(lefts[-1]>rights[-ng]))) {
    return(s)
  }
  # extract the segments
  res <- character(0)
  next_to_take <- 1
  for(i in seq_len(ng)) {
    # look for previous
    if(next_to_take<lefts[[i]]) {
      res <- c(res, substr(s, next_to_take, lefts[[i]]-1))
    }
    # take symbol
    res <- c(res, substr(s, lefts[[i]], rights[[i]]))
    next_to_take <- rights[[i]] + 1
  }
  if(next_to_take<=nc) {
    res <- c(res, substr(s, next_to_take, nc))
  }
  Filter(function(p) {nchar(trimws(p, which="both"))>0}, res)
}
