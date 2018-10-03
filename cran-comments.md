

## Test environments

### Windows

    devtools::build_win()

    * using R Under development (unstable) (2018-10-03 r75387)
    * using platform: x86_64-w64-mingw32 (64-bit)
    * using session charset: ISO8859-1
    * checking for file 'wrapr/DESCRIPTION' ... OK
    * checking extension type ... Package
    * this is package 'wrapr' version '1.6.3'
    * package encoding: UTF-8
    * checking CRAN incoming feasibility ... Note_to_CRAN_maintainers
    Maintainer: 'John Mount <jmount@win-vector.com>'

    Status: OK
    
    
### OSX 

    R CMD check --as-cran wrapr_1.6.3.tar.gz
    
    * using R version 3.5.0 (2018-04-23)
    * using platform: x86_64-apple-darwin15.6.0 (64-bit)
    * using session charset: UTF-8
    * using option ‘--as-cran’
    * checking for file ‘wrapr/DESCRIPTION’ ... OK
    * checking extension type ... Package
    * this is package ‘wrapr’ version ‘1.6.3’
    * package encoding: UTF-8
    * checking CRAN incoming feasibility ... Note_to_CRAN_maintainers
    Maintainer: ‘John Mount <jmount@win-vector.com>’
    * checking top-level files ... WARNING
    Conversion of ‘README.md’ failed:
    pandoc: Could not fetch https://www.r-pkg.org/badges/version/wrapr
    TlsException (HandshakeFailed (Error_Protocol ("expecting server hello, got alert : [(AlertLevel_Fatal,HandshakeFailure)]",True,HandshakeFailure)))
    Status: 1 WARNING

    Warning is spurious (and not repeated on other tests): the URL is good.


## Downstream dependencies

    Checked all downstream dependencies.

    devtools::revdep_check()
    Downloading 8 source packages: cdata, replyr, rqdatatable, rquery, seplyr, sigr, vtreat, WVPlots
    Checked cdata      : 0 errors | 0 warnings | 0 notes
    Checked replyr     : 0 errors | 0 warnings | 0 notes
    Checked rqdatatable: 0 errors | 0 warnings | 0 notes
    Checked rquery     : 0 errors | 0 warnings | 0 notes
    Checked seplyr     : 0 errors | 0 warnings | 0 notes
    Checked sigr       : 0 errors | 0 warnings | 0 notes
    Checked vtreat     : 0 errors | 0 warnings | 0 notes
    Checked WVPlots    : 0 errors | 0 warnings | 0 notes
    
