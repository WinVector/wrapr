

wrapr_deparse <- function(item) {
  paste(as.character(deparse(item, width.cutoff = 500L)),
        collapse = "\n ")
}