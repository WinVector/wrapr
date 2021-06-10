

## Test environments


### Windows

    devtools::check_win_devel()
    * using R Under development (unstable) (2021-06-07 r80458)
    * using platform: x86_64-w64-mingw32 (64-bit)
    * using session charset: ISO8859-1
    * checking for file 'wrapr/DESCRIPTION' ... OK
    * checking extension type ... Package
    * this is package 'wrapr' version '2.0.8'
    * package encoding: UTF-8
    * checking CRAN incoming feasibility ... Note_to_CRAN_maintainers
    Maintainer: 'John Mount <jmount@win-vector.com>'
    ...
    Status: OK
 
### MacOS

    R CMD check --as-cran wrapr_2.0.8.tar.gz
    * using R version 4.0.2 (2020-06-22)
    * using platform: x86_64-apple-darwin17.0 (64-bit)
    * using session charset: UTF-8
    * using option ‘--as-cran’
    * checking for file ‘wrapr/DESCRIPTION’ ... OK
    * checking extension type ... Package
    * this is package ‘wrapr’ version ‘2.0.8’
    * package encoding: UTF-8
    * checking CRAN incoming feasibility ... Note_to_CRAN_maintainers
    Maintainer: ‘John Mount <jmount@win-vector.com>’
    ...
    Status: OK
    
### Linux

    rhub::check_for_cran()
    1345#> * using R Under development (unstable) (2021-06-09 r80471)
    1346#> * using platform: x86_64-pc-linux-gnu (64-bit)
    1347#> * using session charset: UTF-8
    1348#> * using option ‘--as-cran’
    1349#> * checking for file ‘wrapr/DESCRIPTION’ ... OK
    1350#> * checking extension type ... Package
    1351#> * this is package ‘wrapr’ version ‘2.0.8’
    1352#> * package encoding: UTF-8
    1353#> * checking CRAN incoming feasibility ... Note_to_CRAN_maintainers
    1354#> Maintainer: ‘John Mount ’
    1412#> Status: OK

## Downstream dependencies

    Checked all reverse dependencies https://github.com/WinVector/wrapr/blob/master/extras/check_reverse_dependencies.md .
    RccpDynprog failure is wrong compiler on MacOS.

