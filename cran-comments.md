

## Test environments

### Windows

    devtools::build_win()
    * using R version 3.5.2 (2018-12-20)
    * using platform: x86_64-w64-mingw32 (64-bit)
    * using session charset: ISO8859-1
    * checking for file 'wrapr/DESCRIPTION' ... OK
    * checking extension type ... Package
    * this is package 'wrapr' version '1.8.2'
    * package encoding: UTF-8
    * checking CRAN incoming feasibility ... Note_to_CRAN_maintainers
    Maintainer: 'John Mount <jmount@win-vector.com>'
    Status: OK

    rhub::check_for_cran()
    485#> * using R Under development (unstable) (2018-12-26 r75909)
    486#> * using platform: x86_64-w64-mingw32 (64-bit)
    487#> * using session charset: ISO8859-1
    488#> * using option '--as-cran'
    489#> * checking for file 'wrapr/DESCRIPTION' ... OK
    490#> * this is package 'wrapr' version '1.8.2'
    491#> * checking extension type ... Package
    492#> * package encoding: UTF-8
    493#> * checking CRAN incoming feasibility ... Note_to_CRAN_maintainers
    494#> Maintainer: 'John Mount '
    536#> * checking sizes of PDF files under 'inst/doc' ... NOTE
    537#> Unable to find GhostScript executable to run checks on size reduction
    550#> Status: 1 NOTE
    Note is an R-hub problem, not a package problem.


### Linux

    rhub::check_for_cran()
    1129#> * using R version 3.4.4 (2018-03-15)
    1130#> * using platform: x86_64-pc-linux-gnu (64-bit)
    1131#> * using session charset: UTF-8
    1132#> * using option ‘--as-cran’
    1133#> * checking for file ‘wrapr/DESCRIPTION’ ... OK
    1134#> * checking extension type ... Package
    1135#> * this is package ‘wrapr’ version ‘1.8.2’
    1136#> * package encoding: UTF-8
    1137#> * checking CRAN incoming feasibility ... Note_to_CRAN_maintainers
    1138#> Maintainer: ‘John Mount ’
    1192#> Status: OK
    
    rhub::check_for_cran()
    2346#> * using R Under development (unstable) (2018-12-22 r75884)
    2347#> * using platform: x86_64-pc-linux-gnu (64-bit)
    2348#> * using session charset: UTF-8
    2349#> * using option ‘--as-cran’
    2350#> * checking for file ‘wrapr/DESCRIPTION’ ... OK
    2351#> * checking extension type ... Package
    2352#> * this is package ‘wrapr’ version ‘1.8.2’
    2353#> * package encoding: UTF-8
    2354#> * checking CRAN incoming feasibility ... Note_to_CRAN_maintainers
    2355#> Maintainer: ‘John Mount ’
    2411#> Status: OK


## Downstream dependencies

    Checked all downstream dependencies.

    devtools::revdep_check()
    Checking 9 packages: cdata, RcppDynProg, replyr, rqdatatable, rquery, seplyr, sigr, vtreat, WVPlots
    Checked cdata      : 0 errors | 0 warnings | 0 notes
    Checked RcppDynProg: 0 errors | 0 warnings | 0 notes
    Checked replyr     : 0 errors | 0 warnings | 0 notes
    Checked rqdatatable: 0 errors | 0 warnings | 0 notes
    Checked rquery     : 0 errors | 0 warnings | 0 notes
    Checked seplyr     : 0 errors | 0 warnings | 0 notes
    Checked sigr       : 0 errors | 0 warnings | 0 notes
    Checked vtreat     : 0 errors | 0 warnings | 0 notes
    Checked WVPlots    : 0 errors | 0 warnings | 0 notes


