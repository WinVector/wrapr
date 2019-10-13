
`wrapr` fix in response to CRAN email requesting fix 2019-10-13 2:04 AM:

> Please correct before 2019-10-27 to safely retain your package on CRAN.

Example from `help(qchar_frame)` was raising an exception

Notes on underlying issue and diagnosis can be found here: https://github.com/WinVector/wrapr/blob/master/extras/wrapr_dev_CRAN_issue_2019_10_13.md

A follow-up email noted the change in behavior was likely seen in the October 11th, 2019 and later versions, so the Linux check should also see the issue and its fix.

I have confirmed by hand both the presence of the issue in "R Under development (unstable) (2019-10-12 r77279)" and that the fix works on this version.

## Test environments


### Windows

    
    rhub::check_for_cran()
    The GhostScript note is a property of the check installation, not the package.
 
### MacOS

    R CMD check --as-cran wrapr_1.9.2.tar.gz 
    * using R version 3.6.0 (2019-04-26)
    * using platform: x86_64-apple-darwin15.6.0 (64-bit)
    * using session charset: UTF-8
    * using option ‘--as-cran’
    * checking for file ‘wrapr/DESCRIPTION’ ... OK
    * checking extension type ... Package
    * this is package ‘wrapr’ version ‘1.9.2’
    * package encoding: UTF-8
    * checking CRAN incoming feasibility ... Note_to_CRAN_maintainers
    Maintainer: ‘John Mount <jmount@win-vector.com>’
    Status: OK
    
### Linux

    rhub::check_for_cran()
    1148#> * using R Under development (unstable) (2019-10-12 r77279)
    1149#> * using platform: x86_64-pc-linux-gnu (64-bit)
    1150#> * using session charset: UTF-8
    1151#> * using option ‘--as-cran’
    1152#> * checking for file ‘wrapr/DESCRIPTION’ ... OK
    1153#> * checking extension type ... Package
    1154#> * this is package ‘wrapr’ version ‘1.9.2’
    1155#> * package encoding: UTF-8
    1156#> * checking CRAN incoming feasibility ...NB: need Internet access to use CRAN incoming checks
    1157#> Note_to_CRAN_maintainers
    1158#> Maintainer: ‘John Mount ’
    1216#> Status: OK

    rhub::check_for_cran()
    1128#> * using R version 3.6.1 (2019-07-05)
    1129#> * using platform: x86_64-pc-linux-gnu (64-bit)
    1130#> * using session charset: UTF-8
    1131#> * using option ‘--as-cran’
    1132#> * checking for file ‘wrapr/DESCRIPTION’ ... OK
    1133#> * checking extension type ... Package
    1134#> * this is package ‘wrapr’ version ‘1.9.2’
    1135#> * package encoding: UTF-8
    1136#> * checking CRAN incoming feasibility ... Note_to_CRAN_maintainers
    1137#> Maintainer: ‘John Mount ’
    1193#> Status: OK



## Downstream dependencies

    Checked all reverse dependencies https://github.com/WinVector/wrapr/blob/master/extras/check_reverse_dependencies.md .
