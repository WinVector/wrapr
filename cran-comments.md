

## Test environments


### Windows

    rhub::check_for_cran()

    
    devtools::check_win_devel()

 
### MacOS

    R CMD check --as-cran wrapr_2.1.0.tar.gz 

    
### Linux

    rhub::check_for_cran()


## Downstream dependencies

    Checked all reverse dependencies https://github.com/WinVector/wrapr/blob/master/extras/check_reverse_dependencies.md .
    All pass except midfieldr, which fails on lack of suggested package midfielddata (not available from CRAN, and same failure already seen for midfieldr on CRAN copy).

