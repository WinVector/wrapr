
## Test environments

### Windows

    rhub::check_for_cran()
     502#> setting _R_CHECK_FORCE_SUGGESTS_ to false
     503#> setting R_COMPILE_AND_INSTALL_PACKAGES to never
     504#> setting R_REMOTES_STANDALONE to true
     505#> setting R_REMOTES_NO_ERRORS_FROM_WARNINGS to true
     506#> setting _R_CHECK_FORCE_SUGGESTS_ to true
     507#> setting _R_CHECK_CRAN_INCOMING_USE_ASPELL_ to true
     508#> * using log directory 'C:/Users/USERFhYOKjJPCy/wrapr.Rcheck'
     509#> * using R Under development (unstable) (2020-01-07 r77637)
     510#> * using platform: x86_64-w64-mingw32 (64-bit)
     511#> * using session charset: ISO8859-1
     512#> * using option '--as-cran'
     513#> * checking for file 'wrapr/DESCRIPTION' ... OK
     514#> * checking extension type ... Package
     515#> * this is package 'wrapr' version '1.9.4'
     516#> * package encoding: UTF-8
     517#> * checking CRAN incoming feasibility ... Note_to_CRAN_maintainers
     518#> Maintainer: 'John Mount '
     560#> * checking sizes of PDF files under 'inst/doc' ... NOTE
     561#> Unable to find GhostScript executable to run checks on size reduction
     576#> Status: 1 NOTE
    The GhostScript note is a property of the check installation, not the package.
 
### MacOS

    R CMD check --as-cran wrapr_1.9.4.tar.gz
    * using R version 3.6.0 (2019-04-26)
    * using platform: x86_64-apple-darwin15.6.0 (64-bit)
    * using session charset: UTF-8
    * using option ‘--as-cran’
    * checking for file ‘wrapr/DESCRIPTION’ ... OK
    * checking extension type ... Package
    * this is package ‘wrapr’ version ‘1.9.4’
    * package encoding: UTF-8
    * checking CRAN incoming feasibility ... Note_to_CRAN_maintainers
    Maintainer: ‘John Mount <jmount@win-vector.com>’
    Status: OK



## Downstream dependencies

    Checked all reverse dependencies https://github.com/WinVector/wrapr/blob/master/extras/check_reverse_dependencies.md .
