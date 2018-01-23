

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
#' cat(map_to_char(c('a':='b', 'c':='d')))
#'
#' @export
#'
map_to_char <- function(mp,
                        sep = " ",
                        assignment = "=",
                        quote_fn = base::shQuote) {
  nms <- names(mp)
  nv <- vapply(nms,
               function(ni) {
                 paste(quote_fn(ni), assignment, quote_fn(mp[[ni]]))
               }, character(1))
  paste0("c(", paste(nv, collapse = paste0(",", sep)), ")")
}
