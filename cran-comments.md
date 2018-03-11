
Maintenance release: minor additional functionality added
(base functions and data.frame drawing introduced).

## Test environments

  * Windows
  * using R Under development (unstable) (2018-03-09 r74376)
  * using platform: x86_64-w64-mingw32 (64-bit)

  * OSX 
  * using R version 3.4.3 (2017-11-30)
  * using platform: x86_64-apple-darwin15.6.0 (64-bit)


## R CMD check --as-cran wrapr_1.3.0.tar.gz 

  * using option ‘--as-cran’
  * checking for file ‘wrapr/DESCRIPTION’ ... OK
  * checking extension type ... Package
  * this is package ‘wrapr’ version ‘1.3.0’
  * package encoding: UTF-8
  * checking CRAN incoming feasibility ... Note_to_CRAN_maintainers
    Maintainer: ‘John Mount <jmount@win-vector.com>’

Status: OK

## Downstream dependencies

Checked all downstream dependencies.

devtools::revdep_check()

    Checking 5 packages: cdata, replyr, rquery, seplyr, WVPlots
     Checked cdata  : 0 errors | 0 warnings | 0 notes
     Checked replyr : 1 error  | 0 warnings | 0 notes
     Checked rquery : 0 errors | 0 warnings | 0 notes
     Checked seplyr : 0 errors | 0 warnings | 0 notes
     Checked WVPlots: 0 errors | 0 warnings | 0 notes

Need to advance replyr off cdata before next update.
