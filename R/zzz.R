
# https://stackoverflow.com/questions/20223601/r-how-to-run-some-code-on-load-of-package
.onLoad <- function(libname, pkgname){
  # write lambda into package namespace
  do.call('<<-', list(intToUtf8(0x03BB), lambda))
}