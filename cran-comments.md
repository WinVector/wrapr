
Correct unused methods messages in CRAN checks.

## Test environments

    * Windows
    * rhub::check_for_cran(".", show_status = FALSE)
    * using R Under development (unstable) (2018-07-30 r75016)
    * using platform: x86_64-w64-mingw32 (64-bit)
    * checking top-level files ... WARNING
    * Conversion of 'README.md' failed:
    * pandoc.exe: Could not fetch https://www.r-pkg.org/badges/version/wrapr
    * Status: 1 WARNING
    The above warning is suprious: the link is good.  Message does not occur on Linux-check.

    * OSX 
    * R CMD check --as-cran wrapr_1.6.1.tar.gz 
    * using R version 3.5.0 (2018-04-23)
    * using platform: x86_64-apple-darwin15.6.0 (64-bit)
    Status: 1 WARNING
    Conversion of ‘README.md’ failed:
    pandoc: Could not fetch https://www.r-pkg.org/badges/version/wrapr
    The above warning is suprious: the link is good.  Message does not occur on Linux-check.
    
    * Linux
    * rhub::check_for_cran(".", show_status = FALSE)
    * using R version 3.4.4 (2018-03-15)
    * using platform: x86_64-pc-linux-gnu (64-bit)
    Status: OK

## Downstream dependencies

    Checked all downstream dependencies.

    devtools::revdep_check()
    Checking 8 packages: cdata, replyr, rqdatatable, rquery, seplyr, sigr, vtreat, WVPlots
    Checked cdata      : 0 errors | 0 warnings | 0 notes
    Checked replyr     : 0 errors | 0 warnings | 0 notes
    Checked rqdatatable: 0 errors | 0 warnings | 0 notes
    Checked rquery     : 0 errors | 0 warnings | 0 notes
    Checked seplyr     : 0 errors | 0 warnings | 0 notes
    Checked sigr       : 0 errors | 0 warnings | 0 notes
    Checked vtreat     : 0 errors | 0 warnings | 0 notes
    Checked WVPlots    : 0 errors | 0 warnings | 0 notes


 