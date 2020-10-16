

## Test environments

### Windows

    devtools::check_win_devel()
    * using log directory 'd:/RCompile/CRANguest/R-devel/wrapr.Rcheck'
    * using R Under development (unstable) (2020-10-15 r79342)
    * using platform: x86_64-w64-mingw32 (64-bit)
    * using session charset: ISO8859-1
    * checking for file 'wrapr/DESCRIPTION' ... OK
    * checking extension type ... Package
    * this is package 'wrapr' version '2.0.3'
    * package encoding: UTF-8
    * checking CRAN incoming feasibility ... Note_to_CRAN_maintainers
    Maintainer: 'John Mount <jmount@win-vector.com>'
    ...
    Status: OK

    rhub::check_for_cran()
    621#> setting _R_CHECK_FORCE_SUGGESTS_ to false
    622#> setting R_COMPILE_AND_INSTALL_PACKAGES to never
    623#> setting _R_CHECK_THINGS_IN_CHECK_DIR_ to false
    624#> setting R_REMOTES_STANDALONE to true
    625#> setting R_REMOTES_NO_ERRORS_FROM_WARNINGS to true
    626#> setting _R_CHECK_FORCE_SUGGESTS_ to true
    627#> setting _R_CHECK_CRAN_INCOMING_USE_ASPELL_ to true
    628#> * using log directory 'C:/Users/USEROQmosDufHU/wrapr.Rcheck'
    629#> * using R Under development (unstable) (2020-10-09 r79317)
    630#> * using platform: x86_64-w64-mingw32 (64-bit)
    631#> * using session charset: ISO8859-1
    632#> * using option '--as-cran'
    633#> * checking for file 'wrapr/DESCRIPTION' ... OK
    634#> * checking extension type ... Package
    635#> * this is package 'wrapr' version '2.0.3'
    636#> * package encoding: UTF-8
    637#> * checking CRAN incoming feasibility ... Note_to_CRAN_maintainers
    638#> Maintainer: 'John Mount '
    680#> * checking sizes of PDF files under 'inst/doc' ... NOTE
    681#> Unable to find GhostScript executable to run checks on size reduction
    696#> Status: 1 NOTE
    Note is a property of test facility, not the package.

### MacOS

    R CMD check --as-cran wrapr_2.0.3.tar.gz 
    * using log directory ‘/Users/johnmount/Documents/work/wrapr.Rcheck’
    * using R version 4.0.2 (2020-06-22)
    * using platform: x86_64-apple-darwin17.0 (64-bit)
    * using session charset: UTF-8
    * using option ‘--as-cran’
    * checking for file ‘wrapr/DESCRIPTION’ ... OK
    * checking extension type ... Package
    * this is package ‘wrapr’ version ‘2.0.3’
    * package encoding: UTF-8
    * checking CRAN incoming feasibility ... Note_to_CRAN_maintainers
    Maintainer: ‘John Mount <jmount@win-vector.com>’
    ...
    Status: OK


## Downstream dependencies

    Checked all reverse dependencies https://github.com/WinVector/wrapr/blob/master/extras/check_reverse_dependencies.md .

