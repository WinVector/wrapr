

#' format a map.
#'
#' @param mp named vector or list
#' @param sep separator
#' @return character formatted representation
#'
#' @examples
#'
#' cat(map_to_char(c('a':='b', 'c':='d')))
#'
#' @export
#'
map_to_char <- function(mp, sep = "\n  ") {
  nms <- names(mp)
  nv <- vapply(nms,
               function(ni) {
                 paste(shQuote(ni), ':=', shQuote(mp[[ni]]))
               }, character(1))
  paste0("c(", paste(nv, collapse = paste0(",", sep)), ")")
}