

## Test environments

    * Windows
    * using R Under development (unstable) (2018-09-09 r75268)
    * using platform: x86_64-w64-mingw32 (64-bit)
    * checking CRAN incoming feasibility ... NOTE
    Maintainer: 'John Mount <jmount@win-vector.com>'

    Found the following (possibly) invalid URLs:
      URL: https://CRAN.R-project.org/package=pipeR
        From: README.md
       Status: Error
        Message: libcurl error code 7:
      	    Failed to connect to cran.r-project.org port 443: Timed out
      URL: https://www.r-project.org
        From: inst/doc/lambda.html
        Status: Error
       Message: libcurl error code 7:
          	Failed to connect to www.r-project.org port 443: Timed out  
    Status: 1 NOTE
    URLs are good, check NOTE is a false positive.
    
    
    * OSX 
    R CMD check --as-cran wrapr_1.6.2.tar.gz
    * using R version 3.5.0 (2018-04-23)
    * using platform: x86_64-apple-darwin15.6.0 (64-bit)
    * using option ‘--as-cran’
    * checking for file ‘wrapr/DESCRIPTION’ ... OK
    * checking extension type ... Package
    * this is package ‘wrapr’ version ‘1.6.2’
    * package encoding: UTF-8
    * checking CRAN incoming feasibility ... Note_to_CRAN_maintainers
    Maintainer: ‘John Mount <jmount@win-vector.com>’
    * checking top-level files ... WARNING
    Conversion of ‘README.md’ failed:
    pandoc: Could not fetch https://www.r-pkg.org/badges/version/wrapr
    TlsExceptionHostPort (HandshakeFailed (Error_Misc "user error (unexpected type received. expecting handshake and got: Alert [(AlertLevel_Fatal,HandshakeFailure)])")) "www.r-pkg.org" 443
    Fetch WARNING is a false positive on how badges work.
    Status: 1 WARNING


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
 