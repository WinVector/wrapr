

## Test environments

    * Windows
    * using R Under development (unstable) (2018-07-31 r75040)
    * using platform: x86_64-w64-mingw32 (64-bit)

    * OSX 
    * using R version 3.5.0 (2018-04-23)
    * using platform: x86_64-apple-darwin15.6.0 (64-bit)
    Status: 1 WARNING
    Conversion of ‘README.md’ failed:
    pandoc: Could not fetch https://www.r-pkg.org/badges/version/wrapr
    TlsException (HandshakeFailed (Error_Protocol ("expecting server hello, got alert : [(AlertLevel_Fatal,HandshakeFailure)]",True,HandshakeFailure)))
    The above warning is suprious: the link is good.  Message does not occur on win-check.

## R CMD check --as-cran wrapr_1.6.0.tar.gz 

    * using session charset: UTF-8
    * using option ‘--as-cran’
    * checking for file ‘wrapr/DESCRIPTION’ ... OK
    * checking extension type ... Package
    * this is package ‘wrapr’ version ‘1.6.0’
    * package encoding: UTF-8
    * checking CRAN incoming feasibility ... Note_to_CRAN_maintainers
    Maintainer: ‘John Mount <jmount@win-vector.com>’

## Downstream dependencies

    Checked all downstream dependencies.

    devtools::revdep_check()
    Checking 8 packages: cdata, replyr, rqdatatable, rquery, seplyr, sigr, vtreat, WVPlots
    Checked cdata      : 0 errors | 0 warnings | 0 notes
    Checked replyr     : 0 errors | 0 warnings | 0 notes
    Checked rqdatatable: 0 errors | 0 warnings | 0 notes
    Checked rquery     : 0 errors | 0 warnings | 0 notes
    Checked seplyr     : 0 errors | 0 warnings | 0 notes
    Checked sigr       : 0 errors | 0 warnings | 0 notes
    Checked vtreat     : 0 errors | 0 warnings | 0 notes
    Checked WVPlots    : 0 errors | 0 warnings | 0 notes


 