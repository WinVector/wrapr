

## Test environments

### Windows

    devtools::build_win()
    * using R Under development (unstable) (2019-01-31 r76038)
    * using platform: x86_64-w64-mingw32 (64-bit)
    * using session charset: ISO8859-1
    * checking for file 'wrapr/DESCRIPTION' ... OK
    * checking extension type ... Package
    * this is package 'wrapr' version '1.8.4'
    * package encoding: UTF-8
    * checking CRAN incoming feasibility ... NOTE
    Maintainer: 'John Mount <jmount@win-vector.com>'
    Days since last update: 5
    Number of updates in past 6 months: 7
    Status: 1 NOTE

### MacOS

    R CMD check --as-cran wrapr_1.8.4.tar.gz 
    * using R version 3.5.0 (2018-04-23)
    * using platform: x86_64-apple-darwin15.6.0 (64-bit)
    * using session charset: UTF-8
    * using option ‘--as-cran’
    * checking for file ‘wrapr/DESCRIPTION’ ... OK
    * checking extension type ... Package
    * this is package ‘wrapr’ version ‘1.8.4’
    * package encoding: UTF-8
    * checking CRAN incoming feasibility ... NOTE
    Maintainer: ‘John Mount <jmount@win-vector.com>’
    Days since last update: 5
    Number of updates in past 6 months: 7
    * checking top-level files ... WARNING
    Conversion of ‘README.md’ failed:
    pandoc: Could not fetch https://www.r-pkg.org/badges/version/wrapr
    TlsException (HandshakeFailed (Error_Protocol ("expecting server hello, got alert : [(AlertLevel_Fatal,HandshakeFailure)]",True,HandshakeFailure)))
    Status: 1 WARNING, 1 NOTE
   pandoc alert is spurious, link is good.

## Downstream dependencies

    Checked all downstream dependencies.

    devtools::revdep_check()
    Checking 9 packages: cdata, RcppDynProg, replyr, rqdatatable, rquery, seplyr, sigr, vtreat, WVPlots
    Checked cdata      : 0 errors | 0 warnings | 0 notes
    Checked RcppDynProg: 0 errors | 0 warnings | 0 notes
    Checked replyr     : 0 errors | 0 warnings | 0 notes
    Checked rqdatatable: 0 errors | 0 warnings | 0 notes
    Checked rquery     : 0 errors | 0 warnings | 0 notes
    Checked seplyr     : 0 errors | 0 warnings | 0 notes
    Checked sigr       : 0 errors | 0 warnings | 0 notes
    Checked vtreat     : 0 errors | 0 warnings | 0 notes
    Checked WVPlots    : 0 errors | 0 warnings | 0 notes
