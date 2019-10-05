

## Test environments


### Windows

    rhub::check_for_cran()
    517#> * using R Under development (unstable) (2019-09-18 r77193)
    518#> * using platform: x86_64-w64-mingw32 (64-bit)
    519#> * using session charset: ISO8859-1
    520#> * using option '--as-cran'
    521#> * checking for file 'wrapr/DESCRIPTION' ... OK
    522#> * checking extension type ... Package
    523#> * this is package 'wrapr' version '1.9.1'
    524#> * package encoding: UTF-8
    525#> * checking CRAN incoming feasibility ... Note_to_CRAN_maintainers
    526#> Maintainer: 'John Mount '
    568#> * checking sizes of PDF files under 'inst/doc' ... NOTE
    569#> Unable to find GhostScript executable to run checks on size reduction
    584#> Status: 1 NOTE
    Note is a property of the check installation, not the package.
 
### MacOS

    R CMD check --as-cran wrapr_1.9.1.tar.gz 
    * using R version 3.6.0 (2019-04-26)
    * using platform: x86_64-apple-darwin15.6.0 (64-bit)
    * using session charset: UTF-8
    * using option ‘--as-cran’
    * checking for file ‘wrapr/DESCRIPTION’ ... OK
    * checking extension type ... Package
    * this is package ‘wrapr’ version ‘1.9.1’
    * package encoding: UTF-8
    * checking CRAN incoming feasibility ... Note_to_CRAN_maintainers
    Maintainer: ‘John Mount <jmount@win-vector.com>’
    Status: OK

## Downstream dependencies

    Checked all reverse dependencies https://github.com/WinVector/wrapr/blob/master/extras/check_reverse_dependencies.md .
