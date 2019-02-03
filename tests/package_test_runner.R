
if(requireNamespace("RUnit", quietly = TRUE)) {
  pkg = "wrapr"
  library(pkg, character.only = TRUE)
  wrapr::run_package_tests(pkg, verbose = TRUE, require_RUnit_attached = FALSE)
}
