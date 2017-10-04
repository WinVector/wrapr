

## Test environments

 * using R version 3.4.1 (2017-06-30)
 * using platform: x86_64-apple-darwin15.6.0 (64-bit)
 * win-builder (devel and release) devtools::build_win()

## R CMD check --as-cran wrapr_1.0.0.tar.gz

 * using R version 3.4.1 (2017-06-30)
 * using platform: x86_64-apple-darwin15.6.0 (64-bit)
 * using session charset: UTF-8
 * using option ‘--as-cran’
 * checking for file ‘wrapr/DESCRIPTION’ ... OK
 * checking extension type ... Package
 * this is package ‘wrapr’ version ‘1.0.0’
 * package encoding: UTF-8


 * checking CRAN incoming feasibility ... Note_to_CRAN_maintainers
  Maintainer: ‘John Mount <jmount@win-vector.com>’

Status: OK

## Downstream dependencies

Checked all downstream dependnecies.

devtools::revdep_check()

    Checking 4 packages: cdata, replyr, seplyr, WVPlots
     Checked cdata  : 0 errors | 0 warnings | 0 notes
     Checked replyr : 0 errors | 0 warnings | 0 notes
     Checked seplyr : 0 errors | 1 warning  | 0 notes
     Checked WVPlots: 0 errors | 0 warnings | 0 notes
  
