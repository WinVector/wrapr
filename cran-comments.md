

## Test environments


### Linux

    R CMD check --as-cran wrapr_1.8.7.tar.gz 
    pandoc alert is spurious, link is good.


### Windows

    devtools::build_win()

### MacOS

    R CMD check --as-cran wrapr_1.8.7.tar.gz 
    * using R version 3.5.0 (2018-04-23)
    * using platform: x86_64-apple-darwin15.6.0 (64-bit)
    * using session charset: UTF-8
    * using option ‘--as-cran’
    * checking for file ‘wrapr/DESCRIPTION’ ... OK
    * checking extension type ... Package
    * this is package ‘wrapr’ version ‘1.8.7’
    * package encoding: UTF-8
    * checking CRAN incoming feasibility ... NOTE
    Maintainer: ‘John Mount <jmount@win-vector.com>’
    Number of updates in past 6 months: 7
    * checking top-level files ... WARNING
    Conversion of ‘README.md’ failed:
    pandoc: Could not fetch https://www.r-pkg.org/badges/version/wrapr
    TlsException (HandshakeFailed (Error_Protocol ("expecting server hello, got alert : [(AlertLevel_Fatal,HandshakeFailure)]",True,HandshakeFailure)))
    Status: 1 WARNING, 1 NOTE 
    pandoc alert is spurious, link is good.

## Downstream dependencies

    Checked all reverse dependencies https://github.com/WinVector/wrapr/blob/master/extras/check_reverse_dependencies.md .
    ## cdata_1.0.8 started at 2019-04-15 17:04:58 success at 2019-04-15 17:05:19 (1/0/0) 
    ## RcppDynProg_0.1.2 started at 2019-04-15 17:05:19 success at 2019-04-15 17:06:22 (2/0/0) 
    ## replyr_1.0.0 started at 2019-04-15 17:06:22 success at 2019-04-15 17:06:53 (3/0/0) 
    ## rqdatatable_1.1.4 started at 2019-04-15 17:06:53 success at 2019-04-15 17:07:21 (4/0/0) 
    ## rquery_1.3.2 started at 2019-04-15 17:07:21 success at 2019-04-15 17:08:01 (5/0/0) 
    ## seplyr_0.8.3 started at 2019-04-15 17:08:01 success at 2019-04-15 17:08:27 (6/0/0) 
    ## sigr_1.0.5 started at 2019-04-15 17:08:27 success at 2019-04-15 17:08:50 (7/0/0) 
    ## vtreat_1.3.8 started at 2019-04-15 17:08:50 success at 2019-04-15 17:10:01 (8/0/0) 
    ## WVPlots_1.1.0 started at 2019-04-15 17:10:01 success at 2019-04-15 17:10:56 (9/0/0)
    ## Test of wrapr had 9 successes, 0 failures, and 0 skipped packages. 

 