

wrapr_deparse <- function(item) {
  if(missing(item)) {
    stop("saw missing argument, often this is caused by an extra comma")
  }
  paste(as.character(deparse(item, width.cutoff = 500L)),
        collapse = "\n ")
}