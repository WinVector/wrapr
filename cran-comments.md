

## Test environments

 * local OS X install x86_64-apple-darwin13.4.0 (64-bit) 10.12.3
 * using R version 3.4.0 (2017-04-21)
 * win-builder (devel and release) 

## R CMD check --as-cran wrapr_0.2.0.tar.gz 

 * using R version 3.4.0 (2017-04-21)
 * using platform: x86_64-apple-darwin15.6.0 (64-bit)
 * using session charset: UTF-8
 * using option ‘--as-cran’
 * checking for file ‘wrapr/DESCRIPTION’ ... OK
 * checking extension type ... Package
 * this is package ‘wrapr’ version ‘0.2.0’
 * package encoding: UTF-8


Note_to_CRAN_maintainers
Maintainer: ‘John Mount <jmount@win-vector.com>’

Status: OK

## Downstream dependencies

Checked all downstream dependnecies.

  devtools::revdep('wrapr')
  [1] "cdata"   "replyr"  "WVPlots"
