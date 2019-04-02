

## Test environments


### Linux

    R CMD check --as-cran wrapr_1.8.6.tar.gz 
 
    pandoc alert is spurious, link is good.


### Windows

    devtools::build_win()
 
    Status: OK

### MacOS

    R CMD check --as-cran wrapr_1.8.6.tar.gz 
    * using R version 3.5.0 (2018-04-23)
    * using platform: x86_64-apple-darwin15.6.0 (64-bit)
    * using session charset: UTF-8
    * using option ‘--as-cran’
    * checking for file ‘wrapr/DESCRIPTION’ ... OK
    * checking extension type ... Package
    * this is package ‘wrapr’ version ‘1.8.6’
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
    ## cdata_1.0.8 started at 2019-04-02 08:06:37 success at 2019-04-02 08:06:59 (1/0/0) 
    ## RcppDynProg_0.1.2 started at 2019-04-02 08:06:59 success at 2019-04-02 08:08:04 (2/0/0) 
    ## replyr_1.0.0 started at 2019-04-02 08:08:04 success at 2019-04-02 08:08:36 (3/0/0) 
    ## rqdatatable_1.1.4 started at 2019-04-02 08:08:36 success at 2019-04-02 08:09:02 (4/0/0) 
    ## rquery_1.3.2 started at 2019-04-02 08:09:02 success at 2019-04-02 08:09:41 (5/0/0) 
    ## seplyr_0.8.3 started at 2019-04-02 08:09:41 success at 2019-04-02 08:10:04 (6/0/0) 
    ## sigr_1.0.5 started at 2019-04-02 08:10:04 success at 2019-04-02 08:10:26 (7/0/0) 
    ## vtreat_1.3.8 started at 2019-04-02 08:10:26 success at 2019-04-02 08:11:31 (8/0/0) 
    ## WVPlots_1.0.9 started at 2019-04-02 08:11:31 success at 2019-04-02 08:12:27 (9/0/0)
    ## Test of wrapr had 9 successes, 0 failures, and 0 skipped packages. 

 