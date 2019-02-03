
library(wrapr)

if(requireNamespace("RUnit", quietly = TRUE)) {
  library("RUnit")
  run_package_tests("wrapr", verbose = TRUE)
}

if(requireNamespace("testthat", quietly = TRUE)) {
  library("testthat")
  testthat::test_check("wrapr")
}
