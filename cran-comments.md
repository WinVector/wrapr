

## Test environments


### Windows

    devtools::check_win_devel()

    rhub::check_for_cran()
    609#> setting _R_CHECK_FORCE_SUGGESTS_ to false
    610#> setting R_COMPILE_AND_INSTALL_PACKAGES to never
    611#> setting _R_CHECK_THINGS_IN_CHECK_DIR_ to false
    612#> setting R_REMOTES_STANDALONE to true
    613#> setting R_REMOTES_NO_ERRORS_FROM_WARNINGS to true
    614#> setting _R_CHECK_FORCE_SUGGESTS_ to true
    615#> setting _R_CHECK_CRAN_INCOMING_USE_ASPELL_ to true
    616#> * using log directory 'C:/Users/USERHajKTosAYi/wrapr.Rcheck'
    617#> * using R Under development (unstable) (2021-01-24 r79877)
    618#> * using platform: x86_64-w64-mingw32 (64-bit)
    619#> * using session charset: ISO8859-1
    620#> * using option '--as-cran'
    621#> * checking for file 'wrapr/DESCRIPTION' ... OK
    622#> * checking extension type ... Package
    623#> * this is package 'wrapr' version '2.0.7'
    624#> * package encoding: UTF-8
    625#> * checking CRAN incoming feasibility ... Note_to_CRAN_maintainers
    626#> Maintainer: 'John Mount '
    ...
    668#> * checking sizes of PDF files under 'inst/doc' ... NOTE
    669#> Unable to find GhostScript executable to run checks on size reduction
    ...
    684#> Status: 1 NOTE
    Note is a property of test facility, not of the package.
 
### MacOS

    R CMD check --as-cran wrapr_2.0.7.tar.gz 
    * using R version 4.0.2 (2020-06-22)
    * using platform: x86_64-apple-darwin17.0 (64-bit)
    * using session charset: UTF-8
    * using option ‘--as-cran’
    * checking for file ‘wrapr/DESCRIPTION’ ... OK
    * checking extension type ... Package
    * this is package ‘wrapr’ version ‘2.0.7’
    * package encoding: UTF-8
    * checking CRAN incoming feasibility ... Note_to_CRAN_maintainers
    Maintainer: ‘John Mount <jmount@win-vector.com>’
    ...
    Status: OK


## Downstream dependencies

    Checked all reverse dependencies https://github.com/WinVector/wrapr/blob/master/extras/check_reverse_dependencies.md .

