

## Test environments


### Windows

    rhub::check_for_cran()
    401#> * using R Under development (unstable) (2021-12-17 r81389 ucrt)
    402#> * using platform: x86_64-w64-mingw32 (64-bit)
    403#> * using session charset: UTF-8
    404#> * using option '--as-cran'
    405#> * checking for file 'wrapr/DESCRIPTION' ... OK
    406#> * checking extension type ... Package
    452#> * checking sizes of PDF files under 'inst/doc' ... NOTE
    453#> Unable to find GhostScript executable to run checks on size reduction
    466#> * checking for detritus in the temp directory ... NOTE
    467#> Found the following files/directories:
    468#> 'lastMiKTeXException'
    470#> Status: 2 NOTEs
    Both notes are side-effects of test environment, not from the package.
    
    devtools::check_win_devel()
    * using R Under development (unstable) (2022-01-25 r81562 ucrt)
    * using platform: x86_64-w64-mingw32 (64-bit)
    * using session charset: UTF-8
    * checking for file 'wrapr/DESCRIPTION' ... OK
    * checking extension type ... Package
    * this is package 'wrapr' version '2.0.9'
    ...
    Status: OK
 
### MacOS

    R CMD check --as-cran wrapr_2.0.9.tar.gz 
    * using R version 4.1.2 (2021-11-01)
    * using platform: x86_64-apple-darwin17.0 (64-bit)
    ...
    Status: OK
    
### Linux

    rhub::check_for_cran()
    1558#> * using R version 4.1.2 (2021-11-01)
    1559#> * using platform: x86_64-pc-linux-gnu (64-bit)
    1560#> * using session charset: UTF-8
    1561#> * using option ‘--as-cran’
    1562#> * checking for file ‘wrapr/DESCRIPTION’ ... OK
    1563#> * checking extension type ... Package
    1564#> * this is package ‘wrapr’ version ‘2.0.9’
    1625#> Status: OK
    
    rhub::check_for_cran()
    1599#> * using R Under development (unstable) (2022-01-25 r81562)
    1600#> * using platform: x86_64-pc-linux-gnu (64-bit)
    1601#> * using session charset: UTF-8
    1602#> * using option ‘--as-cran’
    1603#> * checking for file ‘wrapr/DESCRIPTION’ ... OK
    1604#> * checking extension type ... Package
    1605#> * this is package ‘wrapr’ version ‘2.0.9’
    1666#> Status: OK

## Downstream dependencies

    Checked all reverse dependencies https://github.com/WinVector/wrapr/blob/master/extras/check_reverse_dependencies.md .
    RccpDynprog failure is wrong compiler on MacOS.
    MultiATSM failure is absence of neldermead package.

