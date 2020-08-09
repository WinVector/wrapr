

## Test environments

### Windows

     rhub::check_for_cran()
     472#> setting _R_CHECK_FORCE_SUGGESTS_ to false
     473#> setting R_COMPILE_AND_INSTALL_PACKAGES to never
     474#> setting _R_CHECK_THINGS_IN_CHECK_DIR_ to false
     475#> setting R_REMOTES_STANDALONE to true
     476#> setting R_REMOTES_NO_ERRORS_FROM_WARNINGS to true
     477#> setting _R_CHECK_FORCE_SUGGESTS_ to true
     478#> setting _R_CHECK_CRAN_INCOMING_USE_ASPELL_ to true
     479#> * using log directory 'C:/Users/USERdlBwIaqbnM/wrapr.Rcheck'
     480#> * using R Under development (unstable) (2020-07-05 r78784)
     481#> * using platform: x86_64-w64-mingw32 (64-bit)
     482#> * using session charset: ISO8859-1
     483#> * using option '--as-cran'
     484#> * checking for file 'wrapr/DESCRIPTION' ... OK
     485#> * checking extension type ... Package
     486#> * this is package 'wrapr' version '2.0.1'
     487#> * package encoding: UTF-8
     488#> * checking CRAN incoming feasibility ... Note_to_CRAN_maintainers
     489#> Maintainer: 'John Mount '
     531#> * checking sizes of PDF files under 'inst/doc' ... NOTE
     532#> Unable to find GhostScript executable to run checks on size reduction
     546#> * DONE
     547#> Status: 1 NOTE
     "Unable to find GhostScript" is a property of the check environment, not the package.
 
### MacOS

    R CMD check --as-cran wrapr_2.0.1.tar.gz
    * using R version 4.0.0 (2020-04-24)
    * using platform: x86_64-apple-darwin17.0 (64-bit)
    * using session charset: UTF-8
    * using option ‘--as-cran’
    * checking for file ‘wrapr/DESCRIPTION’ ... OK
    * checking extension type ... Package
    * this is package ‘wrapr’ version ‘2.0.1’
    * package encoding: UTF-8
    * checking CRAN incoming feasibility ... Note_to_CRAN_maintainers
    Maintainer: ‘John Mount <jmount@win-vector.com>’
    * DONE
    ...
    Status: OK


## Downstream dependencies

    Checked all reverse dependencies https://github.com/WinVector/wrapr/blob/master/extras/check_reverse_dependencies.md .

