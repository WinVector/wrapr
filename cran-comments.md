
## Test environments

### Windows

    rhub::check_for_cran()
     543#> * using R Under development (unstable) (2019-10-19 r77318)
     544#> * using platform: x86_64-w64-mingw32 (64-bit)
     545#> * using session charset: ISO8859-1
     546#> * using option '--as-cran'
     547#> * checking for file 'wrapr/DESCRIPTION' ... OK
     548#> * checking extension type ... Package
     549#> * this is package 'wrapr' version '1.9.3'
     550#> * package encoding: UTF-8
     551#> * checking CRAN incoming feasibility ... Note_to_CRAN_maintainers
     552#> Maintainer: 'John Mount '
     610#> Status: 1 NOTE
    The GhostScript note is a property of the check installation, not the package.
 
### MacOS

    R CMD check --as-cran wrapr_1.9.3.tar.gz 
    * using R version 3.6.0 (2019-04-26)
    * using platform: x86_64-apple-darwin15.6.0 (64-bit)
    * using session charset: UTF-8
    * using option ‘--as-cran’
    * checking for file ‘wrapr/DESCRIPTION’ ... OK
    * checking extension type ... Package
    * this is package ‘wrapr’ version ‘1.9.3’
    * package encoding: UTF-8
    * checking CRAN incoming feasibility ... Note_to_CRAN_maintainers
    Maintainer: ‘John Mount <jmount@win-vector.com>’
    Status: OK


## Downstream dependencies

    Checked all reverse dependencies https://github.com/WinVector/wrapr/blob/master/extras/check_reverse_dependencies.md .
