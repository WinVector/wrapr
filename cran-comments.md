

## Test environments


### Windows

    devtools::check_win_devel()


### MacOS

    R CMD check --as-cran wrapr_1.8.8.tar.gz 
    * using R version 3.6.0 (2019-04-26)
    * using platform: x86_64-apple-darwin15.6.0 (64-bit)
    * using session charset: UTF-8
    * using option ‘--as-cran’
    * checking for file ‘wrapr/DESCRIPTION’ ... OK
    * checking extension type ... Package
    * this is package ‘wrapr’ version ‘1.8.8’
    * package encoding: UTF-8
    * checking CRAN incoming feasibility ... Note_to_CRAN_maintainers
    Maintainer: ‘John Mount <jmount@win-vector.com>’
    * checking top-level files ... WARNING
    Conversion of ‘README.md’ failed:
    pandoc: Could not fetch https://www.r-pkg.org/badges/version/wrapr
    TlsException (HandshakeFailed (Error_Protocol ("expecting server hello, got alert : [(AlertLevel_Fatal,HandshakeFailure)]",True,HandshakeFailure)))
    Status: 1 WARNING
    Warning is spurious, URL https://www.r-pkg.org/badges/version/wrapr is correct and working.

## Downstream dependencies

    Checked all reverse dependencies https://github.com/WinVector/wrapr/blob/master/extras/check_reverse_dependencies.md .
    ## cdata_1.1.0 started at 2019-07-06 08:26:12 success at 2019-07-06 08:26:40 (1/0/0) 
    ## RcppDynProg_0.1.2 started at 2019-07-06 08:26:40 success at 2019-07-06 08:28:11 (2/0/0) 
    ## replyr_1.0.2 started at 2019-07-06 08:28:11 success at 2019-07-06 08:28:57 (3/0/0) 
    ## rqdatatable_1.1.9 started at 2019-07-06 08:28:57 success at 2019-07-06 08:29:25 (4/0/0) 
    ## rquery_1.3.6 started at 2019-07-06 08:29:25 success at 2019-07-06 08:30:13 (5/0/0) 
    ## seplyr_0.8.3 started at 2019-07-06 08:30:13 success at 2019-07-06 08:30:40 (6/0/0) 
    ## sigr_1.0.5 started at 2019-07-06 08:30:40 success at 2019-07-06 08:31:02 (7/0/0) 
    ## vtreat_1.4.2 started at 2019-07-06 08:31:02 success at 2019-07-06 08:32:14 (8/0/0) 
    ## WVPlots_1.1.0 started at 2019-07-06 08:32:14 success at 2019-07-06 08:33:18 (9/0/0)
    ## Test of wrapr had 9 successes, 0 failures, and 0 skipped packages. 
 