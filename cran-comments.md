

## Test environments

### Windows

     rhub::check_for_cran()
     475#> setting _R_CHECK_FORCE_SUGGESTS_ to false
     476#> setting R_COMPILE_AND_INSTALL_PACKAGES to never
     477#> setting R_REMOTES_STANDALONE to true
     478#> setting R_REMOTES_NO_ERRORS_FROM_WARNINGS to true
     479#> setting _R_CHECK_FORCE_SUGGESTS_ to true
     480#> setting _R_CHECK_CRAN_INCOMING_USE_ASPELL_ to true
     481#> * using log directory 'C:/Users/USERzQmnEBqxSM/wrapr.Rcheck'
     482#> * using R Under development (unstable) (2020-03-08 r77917)
     483#> * using platform: x86_64-w64-mingw32 (64-bit)
     484#> * using session charset: ISO8859-1
     485#> * using option '--as-cran'
     486#> * checking for file 'wrapr/DESCRIPTION' ... OK
     487#> * checking extension type ... Package
     488#> * this is package 'wrapr' version '2.0.0'
     489#> * package encoding: UTF-8
     490#> * checking CRAN incoming feasibility ... NOTE
     491#> Maintainer: 'John Mount '
     492#> Found the following (possibly) invalid URLs:
     493#> URL: https://CRAN.R-project.org/package=R.utils
     494#> From: man/grapes-less-than-s-grapes.Rd
     495#> man/grapes-s-greater-than-grapes.Rd
     496#> man/si.Rd
     497#> man/sinterp.Rd
     498#> Status: Error
     499#> Message: libcurl error code 52:
     500#> Empty reply from server
     501#> URL: https://CRAN.R-project.org/package=glue
     502#> From: man/grapes-less-than-s-grapes.Rd
     503#> man/grapes-s-greater-than-grapes.Rd
     504#> man/si.Rd
     505#> man/sinterp.Rd
     506#> Status: Error
     507#> Message: libcurl error code 52:
     508#> Empty reply from server
     509#> URL: https://CRAN.R-project.org/package=rprintf
     510#> From: man/grapes-less-than-s-grapes.Rd
     511#> man/grapes-s-greater-than-grapes.Rd
     512#> man/si.Rd
     513#> man/sinterp.Rd
     514#> Status: Error
     515#> Message: libcurl error code 52:
     516#> Empty reply from server
     558#> * checking sizes of PDF files under 'inst/doc' ... NOTE
     559#> Unable to find GhostScript executable to run checks on size reduction
     573#> * DONE
     574#> Status: 2 NOTEs
     GhostScript NOTES is a property of the check environment, not of the package.
     URLs confirmed as working
 
### MacOS

    R CMD check --as-cran wrapr_2.0.0.tar.gz
    * using R version 3.6.2 (2019-12-12)
    * using platform: x86_64-apple-darwin15.6.0 (64-bit)
    * using session charset: UTF-8
    * using option ‘--as-cran’
    * checking for file ‘wrapr/DESCRIPTION’ ... OK
    * checking extension type ... Package
    * this is package ‘wrapr’ version ‘2.0.0’
    * package encoding: UTF-8
    * checking CRAN incoming feasibility ... Note_to_CRAN_maintainers
    Maintainer: ‘John Mount <jmount@win-vector.com>’
    Status: OK

## Downstream dependencies

    Checked all reverse dependencies https://github.com/WinVector/wrapr/blob/master/extras/check_reverse_dependencies.md .
