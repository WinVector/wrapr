
#' Map symbol names to referenced values if those values are string scalars (else throw).
#'
#' @param ... symbol names mapping to string scalars
#' @return map from original symbol names to new names (names found in the current environment)
#'
#' @examples
#'
#' x <- 'a'
#' y <- 'b'
#' print(mapsyms(x, y))
#' d <- data.frame(a = 1, b = 2)
#' let(mapsyms(x, y), d$x + d$y)
#'
#' @export
#'
mapsyms <- function(...) {
  mapsyms_envir = parent.frame()
  mapsyms_args <- as.list(substitute(list(...))[-1])
  mapsyms_names <- vapply(mapsyms_args,
                          function(ai) {
                            ai <- as.character(ai)
                            if(length(ai)!=1) {
                              stop("wrapr::mapsyms all arguments must be coercible into length-1 strings")
                            }
                            ai
                          },
                          character(1))
  mapsyms_dests <- lapply(mapsyms_names,
                          function(ni) { get(ni, envir=mapsyms_envir)})
  names(mapsyms_dests) <- mapsyms_names
  mapsyms_bads <- vapply(mapsyms_names,
                         function(mavars_key_i) {
                           mapsyms_val_i <- mapsyms_dests[[mavars_key_i]]
                           (length(mapsyms_val_i)!=1) ||
                             (!is.character(mapsyms_val_i))
                         },
                         logical(1))
  if(any(mapsyms_bads)) {
    stop(paste("wrapr::mapsyms columns not mapping to string scalars: ",
               paste(names(mapsyms_bads)[mapsyms_bads])))
  }
  mapsyms_dests
}
