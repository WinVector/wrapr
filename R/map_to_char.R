

#' format a map.
#'
#' @param mp named vector or list
#' @return character formatted representation
#'
#' @examples
#'
#' cat(map_to_char(c('a':='b', 'c':='d')))
#'
#' @export
#'
map_to_char <- function(mp) {
  nms <- names(mp)
  nv <- vapply(nms,
               function(ni) {
                 paste(shQuote(ni), ':=', shQuote(mp[[ni]]))
               }, character(1))
  paste0("c(", paste(nv, collapse = ", "), ")")
}