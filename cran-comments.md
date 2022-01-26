

## Test environments


### Windows

    devtools::check_win_devel()

 
### MacOS

    R CMD check --as-cran wrapr_2.0.9.tar.gz 
    * using R version 4.0.2 (2020-06-22)
    * using platform: x86_64-apple-darwin17.0 (64-bit)
    ...
    Status: OK
    
### Linux

    rhub::check_for_cran()


## Downstream dependencies

    Checked all reverse dependencies https://github.com/WinVector/wrapr/blob/master/extras/check_reverse_dependencies.md .
    RccpDynprog failure is wrong compiler on MacOS.

