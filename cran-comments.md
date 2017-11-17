

## Test environments

 * Windows
 * using R Under development (unstable) (2017-09-12 r73242)
 * using platform: x86_64-w64-mingw32 (64-bit)
 
 * OSX 
 * using R version 3.4.2 (2017-09-28)
 * using platform: x86_64-apple-darwin15.6.0 (64-bit)

## R CMD check --as-cran wrapr_1.0.1.tar.gz

 * using session charset: UTF-8
 * using option ‘--as-cran’
 * checking for file ‘wrapr/DESCRIPTION’ ... OK
 * checking extension type ... Package
 * this is package ‘wrapr’ version ‘1.0.1’
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

The warning in `seplyr` is a `seplyr` vignette failing due to
[`dplyr` issue 2860](https://github.com/tidyverse/dplyr/issues/2860),
and not due to a change in `wrapr`.  I believe this is fixed in 
development-`dplyr` and in addition a confirmed work around for this issue is
in `seplyr` 0.1.6 (also being submitted to CRAN).
