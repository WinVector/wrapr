

## Test environments


### Windows

    devtools::check_win_devel()
    * using R Under development (unstable) (2020-12-04 r79564)
    * using platform: x86_64-w64-mingw32 (64-bit)
    * using session charset: ISO8859-1
    * checking for file 'wrapr/DESCRIPTION' ... OK
    * checking extension type ... Package
    * this is package 'wrapr' version '2.0.6'
    * package encoding: UTF-8
    * checking CRAN incoming feasibility ... Note_to_CRAN_maintainers
    Maintainer: 'John Mount <jmount@win-vector.com>'
    * checking package namespace information ... OK
    ...
    Status: OK

    rhub::check_for_cran()
    607#> setting _R_CHECK_FORCE_SUGGESTS_ to false
    608#> setting R_COMPILE_AND_INSTALL_PACKAGES to never
    609#> setting _R_CHECK_THINGS_IN_CHECK_DIR_ to false
    610#> setting R_REMOTES_STANDALONE to true
    611#> setting R_REMOTES_NO_ERRORS_FROM_WARNINGS to true
    612#> setting _R_CHECK_FORCE_SUGGESTS_ to true
    613#> setting _R_CHECK_CRAN_INCOMING_USE_ASPELL_ to true
    614#> * using log directory 'C:/Users/USERbylfYxeuqH/wrapr.Rcheck'
    615#> * using R Under development (unstable) (2020-11-30 r79529)
    616#> * using platform: x86_64-w64-mingw32 (64-bit)
    617#> * using session charset: ISO8859-1
    618#> * using option '--as-cran'
    619#> * checking for file 'wrapr/DESCRIPTION' ... OK
    620#> * checking extension type ... Package
    621#> * this is package 'wrapr' version '2.0.6'
    622#> * package encoding: UTF-8
    623#> * checking CRAN incoming feasibility ... Note_to_CRAN_maintainers
    624#> Maintainer: 'John Mount '
    666#> * checking sizes of PDF files under 'inst/doc' ... NOTE
    667#> Unable to find GhostScript executable to run checks on size reduction
    682#> Status: 1 NOTE
    Note is a property of test facility, not of the package.
 
### MacOS

    R CMD check --as-cran wrapr_2.0.6.tar.gz 
    * using R version 4.0.2 (2020-06-22)
    * using platform: x86_64-apple-darwin17.0 (64-bit)
    * using session charset: UTF-8
    * using option ‘--as-cran’
    * checking for file ‘wrapr/DESCRIPTION’ ... OK
    * checking extension type ... Package
    * this is package ‘wrapr’ version ‘2.0.6’
    * package encoding: UTF-8
    * checking CRAN incoming feasibility ... Note_to_CRAN_maintainers
    Maintainer: ‘John Mount <jmount@win-vector.com>’
    ...
    Status: OK


## Downstream dependencies

    Checked all reverse dependencies https://github.com/WinVector/wrapr/blob/master/extras/check_reverse_dependencies.md .

