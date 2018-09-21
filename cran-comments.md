

## Test environments

### Windows

    devtools::build_win()
    
    * using log directory 'd:/RCompile/CRANguest/R-devel/wrapr.Rcheck'
    * using R Under development (unstable) (2018-09-20 r75339)
    * using platform: x86_64-w64-mingw32 (64-bit)
    * using session charset: ISO8859-1
    * checking for file 'wrapr/DESCRIPTION' ... OK
    * checking extension type ... Package
    * this is package 'wrapr' version '1.6.3'
    * package encoding: UTF-8
    * checking CRAN incoming feasibility ... NOTE
    Maintainer: 'John Mount <jmount@win-vector.com>'

    Number of updates in past 6 months: 7

    Status: 1 NOTE
    
    
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
    * checking CRAN incoming feasibility ... NOTE
    Maintainer: ‘John Mount <jmount@win-vector.com>’

    Number of updates in past 6 months: 7
    * checking package namespace information ... OK
    Conversion of ‘README.md’ failed:
    pandoc: Could not fetch https://www.r-pkg.org/badges/version/wrapr
     TlsExceptionHostPort (HandshakeFailed (Error_Misc "user error (unexpected type received. expecting handshake and got: Alert [(AlertLevel_Fatal,HandshakeFailure)])")) "www.r-pkg.org" 443
     
    Status: 1 WARNING, 1 NOT
    
Note is the number of builds, warning is spurious (URL is good).



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
 