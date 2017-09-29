
#' Map varaible names to referenced values if those values are string scalars (else throw).
#'
#' @param ... varaible names mapping to string scalars
#' @return map from original variable names to new names
#'
#' @examples
#'
#' x <- 'a'
#' y <- 'b'
#' mapvars(x, y)
#' d <- data.frame(a = 1, b = 2)
#' let(mapvars(x, y), d$x + d$y)
#'
#' @export
#'
mapvars <- function(...) {
  mapvars_names <- vapply(substitute(list(...))[-1],
                          as.character,
                          character(1))
  mapvars_dests <- lapply(mapvars_names, get)
  names(mapvars_dests) <- mapvars_names
  mapvars_bads <- vapply(mapvars_names,
                         function(mavars_key_i) {
                           mapvars_val_i <- mapvars_dests[[mavars_key_i]]
                           (length(mapvars_val_i)!=1) || (!is.character(mapvars_val_i))
                         },
                         logical(1))
  if(any(mapvars_bads)) {
    stop(paste("wrapr::mapvars columns not mapping to string scalars: ",
               paste(names(mapvars_bads)[mapvars_bads])))
  }
  mapvars_dests
}
