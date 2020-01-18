
This is a quick fix to undo the check-problem my 1.9.4 release caused by using the s= option in sQuote(), which is not available in older Rs.
Please consider accepting this even though it is a quick re-release.

## Test environments

### Windows

    rhub::check_for_cran()
     603#> setting _R_CHECK_FORCE_SUGGESTS_ to false
     604#> setting R_COMPILE_AND_INSTALL_PACKAGES to never
     605#> setting R_REMOTES_STANDALONE to true
     606#> setting R_REMOTES_NO_ERRORS_FROM_WARNINGS to true
     607#> setting _R_CHECK_FORCE_SUGGESTS_ to true
     608#> setting _R_CHECK_CRAN_INCOMING_USE_ASPELL_ to true
     609#> * using log directory 'C:/Users/USERcbRUGPhJHI/wrapr.Rcheck'
     610#> * using R Under development (unstable) (2020-01-07 r77637)
     611#> * using platform: x86_64-w64-mingw32 (64-bit)
     612#> * using session charset: ISO8859-1
     613#> * using option '--as-cran'
     614#> * checking for file 'wrapr/DESCRIPTION' ... OK
     615#> * checking extension type ... Package
     616#> * this is package 'wrapr' version '1.9.5'
     617#> * package encoding: UTF-8
     618#> * checking CRAN incoming feasibility ... NOTE
     619#> Maintainer: 'John Mount '
     620#> Days since last update: 1
     662#> * checking sizes of PDF files under 'inst/doc' ... NOTE
     663#> Unable to find GhostScript executable to run checks on size reduction 
        The GhostScript note is a property of the check installation, not the package.
     678#> Status: 2 NOTEs
 
### MacOS

    R CMD check --as-cran wrapr_1.9.5.tar.gz 
    * using R version 3.6.0 (2019-04-26)
    * using platform: x86_64-apple-darwin15.6.0 (64-bit)
    * using session charset: UTF-8
    * using option ‘--as-cran’
    * checking for file ‘wrapr/DESCRIPTION’ ... OK
    * checking extension type ... Package
    * this is package ‘wrapr’ version ‘1.9.5’
    * package encoding: UTF-8
    * checking CRAN incoming feasibility ... NOTE
    Maintainer: ‘John Mount <jmount@win-vector.com>’
    Days since last update: 1
    Status: 1 NOTE

## Downstream dependencies

    Checked all reverse dependencies https://github.com/WinVector/wrapr/blob/master/extras/check_reverse_dependencies.md .
