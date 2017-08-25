


# https://stackoverflow.com/questions/20223601/r-how-to-run-some-code-on-load-of-package
# https://stat.ethz.ch/R-manual/R-devel/library/base/html/userhooks.html
.onAttach <- function(libname, pkgname){
  # write lambda into namespace
  # would prefer to write into package namespace
  do.call('<<-', list(intToUtf8(0x03BB), lambda))
}