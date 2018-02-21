

Note to CRAN:

There is one reverse dependency check issue: 'seplyr' issues 1 warning,
which is due to a vignette failure.  This failure is already occurring
on CRAN and was triggered by the release of version 0.2.0 of 
the 'rlang' package (published 2018-02-20).  It is my intent to 
fix 'seplyr' by removing offending code, but that is much easier
and safer to do if I can update the dependent package 'wrapr' first 
(which is what I am attempting to do with this 'wrapr' release).


## Test environments

 * Windows
 * using R Under development (unstable) (2018-02-20 r74280)
 * using platform: x86_64-w64-mingw32 (64-bit)

 * OSX 
 * using R version 3.4.3 (2017-11-30)
 * using platform: x86_64-apple-darwin15.6.0 (64-bit)


## R CMD check --as-cran wrapr_1.2.0.tar.gz 

 * this is package ‘wrapr’ version ‘1.2.0’

 * checking CRAN incoming feasibility ... NOTE
   Maintainer: ‘John Mount <jmount@win-vector.com>’

Status: OK


## Downstream dependencies

Checked all downstream dependnecies.

devtools::revdep_check()

    Checking 5 packages: cdata, replyr, rquery, seplyr, WVPlots
    Checked cdata  : 0 errors | 0 warnings | 0 notes
    Checked replyr : 0 errors | 0 warnings | 0 notes
    Checked rquery : 0 errors | 0 warnings | 0 notes
    Checked seplyr : 0 errors | 1 warning  | 0 notes
    Checked WVPlots: 0 errors | 0 warnings | 0 notes
