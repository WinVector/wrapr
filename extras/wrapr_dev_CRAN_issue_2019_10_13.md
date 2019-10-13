wrapr\_CRAN\_dev\_issue\_2019\_10\_13
================

`wrapr` fix in response to CRAN email requesting fix 2019-10-13 2:04 AM:

> Please correct before 2019-10-27 to safely retain your package on
> CRAN.

``` r
# It appears wrapr 1.9.1's issue on 2019-10-13 development R was wrapr was
# improperly depending on correct performance of bquote() on a mal-formed
# language item.
# The symptom is reproducible here even in "R version 3.6.0 (2019-04-26)".

loss_name <- "loss"

f <- function(...) {
  env <- parent.frame()
  v <- do.call(bquote, list(as.list(substitute(list(...))[-1]),
                            where = env),
               envir = env)
  return(v)
}

f( x, training |
   y,  .(loss_name) )
```

    ## x(training | y, "loss")

``` r
f( training |
    .(loss_name) )
```

    ## [[1]]
    ## training | .(loss_name)

``` r
# Notice how one of the above evaluations of f() replaced
# .(loss_name) with "loss" and one did not. wrapr was
# (incorrectly) assuming the substitution would always take
# place.  The issue appears to be that taking the first
# entry off a call language item as a bad thing to do.
# The following cleaner code works in both cases for
# "R version 3.6.0 (2019-04-26)" and for
# "R Under development (unstable) (2019-10-12 r77279)"

f2 <- function(...) {
  env <- parent.frame()
  v <- do.call(bquote, list(substitute(alist(...)),
                            where = env),
               envir = env)
  v <- lapply(2:length(v), function(i) {v[[i]]})
  return(v)
}

f2( x, training |
      y,  .(loss_name) )
```

    ## [[1]]
    ## x
    ## 
    ## [[2]]
    ## training | y
    ## 
    ## [[3]]
    ## [1] "loss"

``` r
f2( training |
      .(loss_name) )
```

    ## [[1]]
    ## training | "loss"
