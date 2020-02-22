

## Test environments

### Windows

    rhub::check_for_cran()
     477#> setting _R_CHECK_FORCE_SUGGESTS_ to false
     478#> setting R_COMPILE_AND_INSTALL_PACKAGES to never
     479#> setting R_REMOTES_STANDALONE to true
     480#> setting R_REMOTES_NO_ERRORS_FROM_WARNINGS to true
     481#> setting _R_CHECK_FORCE_SUGGESTS_ to true
     482#> setting _R_CHECK_CRAN_INCOMING_USE_ASPELL_ to true
     483#> * using log directory 'C:/Users/USERJrHDeoAxLS/wrapr.Rcheck'
     484#> * using R Under development (unstable) (2020-01-22 r77697)
     485#> * using platform: x86_64-w64-mingw32 (64-bit)
     486#> * using session charset: ISO8859-1
     487#> * using option '--as-cran'
     488#> * checking for file 'wrapr/DESCRIPTION' ... OK
     489#> * checking extension type ... Package
     490#> * this is package 'wrapr' version '2.0.0'
     491#> * package encoding: UTF-8
     492#> * checking CRAN incoming feasibility ... NOTE
     493#> Maintainer: 'John Mount '
     494#> Number of updates in past 6 months: 7
     536#> * checking sizes of PDF files under 'inst/doc' ... NOTE
     537#> Unable to find GhostScript executable to run checks on size reduction
     552#> Status: 2 NOTEs
     GhostScript NOTES is a property of the check environment, not of the package.
 
### MacOS

    R CMD check --as-cran wrapr_2.0.0.tar.gz
    * using R version 3.6.2 (2019-12-12)
    * using platform: x86_64-apple-darwin15.6.0 (64-bit)
    * using session charset: UTF-8
    * using option ‘--as-cran’
    * checking for file ‘wrapr/DESCRIPTION’ ... OK
    * checking extension type ... Package
    * this is package ‘wrapr’ version ‘2.0.0’
    * package encoding: UTF-8
    * checking CRAN incoming feasibility ... NOTE
    Maintainer: ‘John Mount <jmount@win-vector.com>’
    Number of updates in past 6 months: 7
    Status: 1 NOTE

## Downstream dependencies

    Checked all reverse dependencies https://github.com/WinVector/wrapr/blob/master/extras/check_reverse_dependencies.md .
