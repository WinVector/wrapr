

## Test environments


### Linux

    R CMD check --as-cran wrapr_1.8.5.tar.gz 
    pandoc alert is spurious, link is good.


### Windows

    devtools::build_win()
 

### MacOS

    R CMD check --as-cran wrapr_1.8.5.tar.gz 


## Downstream dependencies

    Checked all reverse dependencies https://github.com/WinVector/wrapr/blob/master/extras/check_reverse_dependencies.md .
