
#' Map symbol names to referenced values if those values are string scalars (else throw).
#'
#' @param ... symbol names mapping to string scalars
#' @return map from original symbol names to new names (names found in the current environment)
#'
#' @seealso \code{\link{let}}
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
  mapsyms_envir <- parent.frame()
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


#' Map up-cased symbol names to referenced values if those values are string scalars (else throw).
#'
#' @param ... symbol names mapping to string scalars
#' @return map from original symbol names to new names (names found in the current environment)
#'
#' @seealso \code{\link{let}}
#'
#' @examples
#'
#' x <- 'a'
#' print(map_upper(x))
#' d <- data.frame(a = "a_val")
#' let(map_upper(x), paste(d$X, x))
#'
#' @export
#'
map_upper <- function(...) {
  map_upper_envir <- parent.frame()
  map_upper_args <- as.list(substitute(list(...))[-1])
  map_upper_names <- vapply(map_upper_args,
                            function(ai) {
                              ai <- as.character(ai)
                              if(length(ai)!=1) {
                                stop("wrapr::map_upper all arguments must be coercible into length-1 strings")
                              }
                              ai
                            },
                            character(1))
  map_upper_dests <- lapply(map_upper_names,
                            function(ni) { get(ni, envir=map_upper_envir)})
  names(map_upper_dests) <- map_upper_names
  map_upper_bads <- vapply(map_upper_names,
                           function(mavars_key_i) {
                             map_upper_val_i <- map_upper_dests[[mavars_key_i]]
                             (length(map_upper_val_i)!=1) ||
                               (!is.character(map_upper_val_i))
                           },
                           logical(1))
  if(any(map_upper_bads)) {
    stop(paste("wrapr::map_upper columns not mapping to string scalars: ",
               paste(names(map_upper_bads)[map_upper_bads])))
  }
  names(map_upper_dests) <- toupper(names(map_upper_dests))
  map_upper_dests
}

