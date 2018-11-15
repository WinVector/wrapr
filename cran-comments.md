

## Test environments

### Windows

    devtools::build_win()
    * using R Under development (unstable) (2018-11-14 r75608)
    * using platform: x86_64-w64-mingw32 (64-bit)
    * using session charset: ISO8859-1
    * checking for file 'wrapr/DESCRIPTION' ... OK
    * checking extension type ... Package
    * this is package 'wrapr' version '1.7.0'
    * package encoding: UTF-8
    * checking CRAN incoming feasibility ... Note_to_CRAN_maintainers
    Maintainer: 'John Mount <jmount@win-vector.com>'
    Status: OK

    rhub::check_for_cran()
    460#> * using R Under development (unstable) (2018-09-27 r75377)
    461#> * using platform: x86_64-w64-mingw32 (64-bit)
    462#> * using session charset: ISO8859-1
    463#> * using option '--as-cran'
    464#> * checking for file 'wrapr/DESCRIPTION' ... OK
    465#> * checking extension type ... Package
    466#> * this is package 'wrapr' version '1.7.0'
    467#> * package encoding: UTF-8
    468#> * checking CRAN incoming feasibility ... Note_to_CRAN_maintainers
    469#> Maintainer: 'John Mount '
    511#> * checking sizes of PDF files under 'inst/doc' ... NOTE
    512#> Unable to find GhostScript executable to run checks on size reduction
    525#> Status: 1 NOTE
  
### OSX 

    R CMD check --as-cran wrapr_1.7.0.tar.gz
    * using R version 3.5.0 (2018-04-23)
    * using platform: x86_64-apple-darwin15.6.0 (64-bit)
    * using session charset: UTF-8
    * using option ‘--as-cran’
    * checking for file ‘wrapr/DESCRIPTION’ ... OK
    * checking extension type ... Package
    * this is package ‘wrapr’ version ‘1.7.0’
    * package encoding: UTF-8
    * checking CRAN incoming feasibility ...
    Maintainer: ‘John Mount <jmount@win-vector.com>’
    Conversion of ‘README.md’ failed:
    pandoc: Could not fetch https://www.r-pkg.org/badges/version/wrapr
    TlsException (HandshakeFailed (Error_Protocol ("expecting server hello, got alert : [(AlertLevel_Fatal,HandshakeFailure)]",True,HandshakeFailure)))
    Status: 1 WARNING
    Warning is spurious (and not repeated on other tests): the URL is good.

### Linux

    R CMD check --as-cran wrapr_1.7.0.tar.gz
    using R version 3.5.1 (2018-07-02)
    * using platform: x86_64-pc-linux-gnu (64-bit)
    * using session charset: UTF-8
    * using option ‘--as-cran’
    * checking for file ‘wrapr/DESCRIPTION’ ... OK
    * checking extension type ... Package
    * this is package ‘wrapr’ version ‘1.7.0’
    * package encoding: UTF-8
    * checking CRAN incoming feasibility ... Note_to_CRAN_maintainers
    Maintainer: ‘John Mount <jmount@win-vector.com>’
    Conversion of ‘README.md’ failed:
    pandoc: Could not fetch https://www.r-pkg.org/badges/version/wrapr
    TlsExceptionHostPort (HandshakeFailed (Error_Misc "user error (unexpected type received. expecting handshake and got: Alert [(AlertLevel_Fatal,HandshakeFailure)])")) "www.r-pkg.org" 443
    Status: 1 WARNING
    Warning is spurious (and not repeated on other tests): the URL is good.

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

    
