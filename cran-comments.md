

## Test environments

### Windows

    devtools::build_win()
  

    rhub::check_for_cran()

  
### OSX 

    R CMD check --as-cran wrapr_1.8.0.tar.gz
    

### Linux

    R CMD check --as-cran wrapr_1.8.0.tar.gz


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


    
