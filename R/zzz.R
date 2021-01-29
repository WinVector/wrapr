


.onAttach <- function(libname, pkgname) {
  wrapr_default_options <- list(
    wrapr.bc.alphabet = paste(c(LETTERS, letters), collapse = ''),
    wrapr.bc.seps = ',|'
  )
  op <- options()
  toset <- setdiff(names(wrapr_default_options), names(op))
  if(length(toset)>0) {
    options(wrapr_default_options[toset])
  }
  invisible()
}
