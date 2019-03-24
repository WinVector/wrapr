

## Test environments


### Linux

    R CMD check --as-cran wrapr_1.8.5.tar.gz 
    * using R version 3.5.2 (2018-12-20)
    * using platform: x86_64-pc-linux-gnu (64-bit)
    * using session charset: UTF-8
    * using option ‘--as-cran’
    * checking for file ‘wrapr/DESCRIPTION’ ... OK
    * checking extension type ... Package
    * this is package ‘wrapr’ version ‘1.8.5’
    * package encoding: UTF-8
    * checking CRAN incoming feasibility ... Note_to_CRAN_maintainers
    Maintainer: ‘John Mount <jmount@win-vector.com>’
    * checking top-level files ... WARNING
    Conversion of ‘README.md’ failed:
    pandoc: Could not fetch https://www.r-pkg.org/badges/version/wrapr
    TlsExceptionHostPort (HandshakeFailed (Error_Misc "user error (unexpected type received. expecting handshake and got: Alert [(AlertLevel_Fatal,HandshakeFailure)])")) "www.r-pkg.org" 443
    Status: 1 WARNING
    pandoc alert is spurious, link is good.


### Windows

    devtools::build_win()
    * using R Under development (unstable) (2019-03-22 r76262)
    * using platform: x86_64-w64-mingw32 (64-bit)
    * using session charset: ISO8859-1
    * checking for file 'wrapr/DESCRIPTION' ... OK
    * checking extension type ... Package
    * this is package 'wrapr' version '1.8.5'
    * package encoding: UTF-8
    * checking CRAN incoming feasibility ... Note_to_CRAN_maintainers
    Maintainer: 'John Mount <jmount@win-vector.com>'
    Status: OK

### MacOS

    R CMD check --as-cran wrapr_1.8.5.tar.gz 
    * using R version 3.5.0 (2018-04-23)
    * using platform: x86_64-apple-darwin15.6.0 (64-bit)
    * using session charset: UTF-8
    * using option ‘--as-cran’
    * checking for file ‘wrapr/DESCRIPTION’ ... OK
    * checking extension type ... Package
    * this is package ‘wrapr’ version ‘1.8.5’
    * package encoding: UTF-8
    * checking CRAN incoming feasibility ... Note_to_CRAN_maintainers
    Maintainer: ‘John Mount <jmount@win-vector.com>’
    * checking top-level files ... WARNING
    Conversion of ‘README.md’ failed:
    pandoc: Could not fetch https://www.r-pkg.org/badges/version/wrapr
    TlsException (HandshakeFailed (Error_Protocol ("expecting server hello, got alert : [(AlertLevel_Fatal,HandshakeFailure)]",True,HandshakeFailure)))
    Status: 1 WARNING
    pandoc alert is spurious, link is good.

## Downstream dependencies

    Checked all reverse dependencies https://github.com/WinVector/wrapr/blob/master/extras/check_reverse_dependencies.md .
    ## cdata_1.0.7 started at 2019-03-24 15:15:52 success at 2019-03-24 15:16:12 (1/0/0) 
    ## RcppDynProg_0.1.1 started at 2019-03-24 15:16:12 success at 2019-03-24 15:17:19 (2/0/0) 
    ## replyr_0.9.9 started at 2019-03-24 15:17:19 success at 2019-03-24 15:17:48 (3/0/0) 
    ## rqdatatable_1.1.4 started at 2019-03-24 15:17:48 success at 2019-03-24 15:18:12 (4/0/0) 
    ## rquery_1.3.2 started at 2019-03-24 15:18:12 success at 2019-03-24 15:18:48 (5/0/0) 
    ## seplyr_0.8.3 started at 2019-03-24 15:18:48 success at 2019-03-24 15:19:12 (6/0/0) 
    ## sigr_1.0.5 started at 2019-03-24 15:19:12 success at 2019-03-24 15:19:34 (7/0/0) 
    ## vtreat_1.3.7 started at 2019-03-24 15:19:34 success at 2019-03-24 15:20:45 (8/0/0) 
    ## WVPlots_1.0.9 started at 2019-03-24 15:20:45 success at 2019-03-24 15:21:50 (9/0/0)
    ## Test of wrapr had 9 successes, 0 failures, and 0 skipped packages. 
