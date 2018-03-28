

#' format a map.
#'
#' @param mp named vector or list
#' @param sep separator suffix, what to put after commas
#' @param assignment assignment string
#' @param quote_fn string quoting function
#' @return character formatted representation
#'
#' @examples
#'
#' cat(map_to_char(c('a' = 'b', 'c' = 'd')))
#' cat(map_to_char(c('a' = 'b', 'd', 'e' = 'f')))
#'
#' @export
#'
map_to_char <- function(mp,
                        sep = " ",
                        assignment = "=",
                        quote_fn = base::shQuote) {
  nms <- names(mp)
  vls <- as.character(mp)
  n <- length(vls)
  if(length(nms)<n) {
    nms <- c(nms, rep("", n - length(nms)))
  }
  nv <- character(n)
  for(i in seq_len(n)) {
    nmi <- nms[[i]]
    vli <- vls[[i]]
    if((!is.na(nmi))&&(nchar(nmi)>0)) {
      nv[[i]] <- paste(quote_fn(nmi), assignment, quote_fn(vli))
    } else {
      nv[[i]] <- quote_fn(vli)
    }
  }
  paste0("c(", paste(nv, collapse = paste0(",", sep)), ")")
}
