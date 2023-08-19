
## Tests

    devtools::check_win_devel()
    * using R Under development (unstable) (2023-08-18 r84986 ucrt)
    * using platform: x86_64-w64-mingw32


    R CMD check --as-cran wrapr_2.1.0.tar.gz 
    * using R version 4.3.0 (2023-04-21)
    * using platform: x86_64-apple-darwin20 (64-bit)

    rhub::check_for_cran()
    skipped


## Downstream dependencies

    Checked all reverse dependencies https://github.com/WinVector/wrapr/blob/master/extras/check_reverse_dependencies.md .
    All pass except midfieldr, which fails on lack of suggested package midfielddata (not available from CRAN, and same failure already seen for midfieldr on CRAN copy).

