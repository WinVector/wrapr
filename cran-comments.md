

## Test environments


### Windows

    devtools::build_win()
    * using R version 3.6.0 (2019-04-26)
    * using platform: x86_64-w64-mingw32 (64-bit)
    Status: OK

### MacOS

    R CMD check --as-cran wrapr_1.8.7.tar.gz 
    * using R version 3.6.0 (2019-04-26)
    * using platform: x86_64-apple-darwin15.6.0 (64-bit)
    * using session charset: UTF-8
    * using option ‘--as-cran’
    * checking for file ‘wrapr/DESCRIPTION’ ... OK
    * checking extension type ... Package
    * this is package ‘wrapr’ version ‘1.8.7’
    * package encoding: UTF-8
    * checking CRAN incoming feasibility ... Note_to_CRAN_maintainers
    Maintainer: ‘John Mount <jmount@win-vector.com>’
    * checking top-level files ... WARNING
    Conversion of ‘README.md’ failed:
    pandoc: Could not fetch https://www.r-pkg.org/badges/version/wrapr
    TlsException (HandshakeFailed (Error_Protocol ("expecting server hello, got alert : [(AlertLevel_Fatal,HandshakeFailure)]",True,HandshakeFailure)))
    Status: 1 WARNING
    pandoc WARNING alert is spurious, link is good.

## Downstream dependencies

    Checked all reverse dependencies https://github.com/WinVector/wrapr/blob/master/extras/check_reverse_dependencies.md .
    ## cdata_1.1.0 started at 2019-05-23 16:41:40 success at 2019-05-23 16:42:06 (1/0/0) 
    ## RcppDynProg_0.1.2 started at 2019-05-23 16:42:06 success at 2019-05-23 16:43:54 (2/0/0) 
    ## replyr_1.0.0 started at 2019-05-23 16:43:55 success at 2019-05-23 16:44:28 (3/0/0) 
    ## rqdatatable_1.1.7 started at 2019-05-23 16:44:28 success at 2019-05-23 16:44:54 (4/0/0) 
    ## rquery_1.3.2 started at 2019-05-23 16:44:54 success at 2019-05-23 16:45:42 (5/0/0) 
    ## seplyr_0.8.3 started at 2019-05-23 16:45:42 success at 2019-05-23 16:46:08 (6/0/0) 
    ## sigr_1.0.5 started at 2019-05-23 16:46:08 success at 2019-05-23 16:46:29 (7/0/0) 
    ## vtreat_1.4.0 started at 2019-05-23 16:46:29 success at 2019-05-23 16:47:42 (8/0/0) 
    ## WVPlots_1.1.0 started at 2019-05-23 16:47:42 success at 2019-05-23 16:48:54 (9/0/0)
    ## Test of wrapr had 9 successes, 0 failures, and 0 skipped packages. 

 