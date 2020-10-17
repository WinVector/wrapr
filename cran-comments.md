

## Test environments

I do not see detritus in the temp directory when I check. I feel the last submission that saw detritus in the Debian check may have beena spurious detection.


### Debian

    R CMD check --as-cran wrapr_2.0.4.tar.gz
    * using log directory ‘/home/parallels/wrapr.Rcheck’
    * using R version 4.0.3 (2020-10-10)
    * using platform: x86_64-pc-linux-gnu (64-bit)
    * using session charset: UTF-8
    * using option ‘--as-cran’
    * checking for file ‘wrapr/DESCRIPTION’ ... OK
    * checking extension type ... Package
    * this is package ‘wrapr’ version ‘2.0.4’
    * package encoding: UTF-8
    * checking CRAN incoming feasibility ... Note_to_CRAN_maintainers
    Maintainer: ‘John Mount <jmount@win-vector.com>’
    * checking package namespace information ... OK
    * checking package dependencies ... OK
    * checking if this is a source package ... OK
    * checking if there is a namespace ... OK
    * checking for executable files ... OK
    * checking for hidden files and directories ... OK
    * checking for portable file names ... OK
    * checking for sufficient/correct file permissions ... OK
    * checking serialization versions ... OK
    * checking whether package ‘wrapr’ can be installed ... OK
    * checking installed package size ... OK
    * checking package directory ... OK
    * checking for future file timestamps ... OK
    * checking ‘build’ directory ... OK
    * checking DESCRIPTION meta-information ... OK
    * checking top-level files ... OK
    * checking for left-over files ... OK
    * checking index information ... OK
    * checking package subdirectories ... OK
    * checking R files for non-ASCII characters ... OK
    * checking R files for syntax errors ... OK
    * checking whether the package can be loaded ... OK
    * checking whether the package can be loaded with stated dependencies ... OK
    * checking whether the package can be unloaded cleanly ... OK
    * checking whether the namespace can be loaded with stated dependencies ... OK
    * checking whether the namespace can be unloaded cleanly ... OK
    * checking loading without being on the library search path ... OK
    * checking use of S3 registration ... OK
    * checking dependencies in R code ... OK
    * checking S3 generic/method consistency ... OK
    * checking replacement functions ... OK
    * checking foreign function calls ... OK
    * checking R code for possible problems ... OK
    * checking Rd files ... OK
    * checking Rd metadata ... OK
    * checking Rd line widths ... OK
    * checking Rd cross-references ... OK
    * checking for missing documentation entries ... OK
    * checking for code/documentation mismatches ... OK
    * checking Rd \usage sections ... OK
    * checking Rd contents ... OK
    * checking for unstated dependencies in examples ... OK
    * checking sizes of PDF files under ‘inst/doc’ ... OK
    * checking installed files from ‘inst/doc’ ... OK
    * checking files in ‘vignettes’ ... OK
    * checking examples ... OK
    * checking for unstated dependencies in ‘tests’ ... OK
    * checking tests ...
      Running ‘tinytest.R’
     OK
    * checking for unstated dependencies in vignettes ... OK
    * checking package vignettes in ‘inst/doc’ ... OK
    * checking re-building of vignette outputs ... OK
    * checking PDF version of manual ... OK
    * checking for non-standard things in the check directory ... OK
    * checking for detritus in the temp directory ... OK
    * DONE
    Status: OK


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

    R CMD check --as-cran wrapr_2.0.4.tar.gz 
    * using log directory ‘/Users/johnmount/Documents/work/wrapr.Rcheck’
    * using R version 4.0.2 (2020-06-22)
    * using platform: x86_64-apple-darwin17.0 (64-bit)
    * using session charset: UTF-8
    * using option ‘--as-cran’
    * checking for file ‘wrapr/DESCRIPTION’ ... OK
    * checking extension type ... Package
    * this is package ‘wrapr’ version ‘2.0.4’
    * package encoding: UTF-8
    * checking CRAN incoming feasibility ... Note_to_CRAN_maintainers
    Maintainer: ‘John Mount <jmount@win-vector.com>’
    ...
    Status: OK



## Downstream dependencies

    Checked all reverse dependencies https://github.com/WinVector/wrapr/blob/master/extras/check_reverse_dependencies.md .

