VectorizeM Example
================

``` r
d <- rep(base::date(), 3)

f1 <- function(d) {
  lubridate::parse_date_time(d, "mdHMSy")
}

str(f1(d))
```

    ##  POSIXct[1:3], format: "2018-10-11 12:51:27" "2018-10-11 12:51:27" ...

``` r
f2 <- wrapr::VectorizeM(f1, 
                        SIMPLIFY = FALSE, 
                        UNLIST = TRUE)

str(f2(d))
```

    ##  POSIXct[1:3], format: "2018-10-11 12:51:27" "2018-10-11 12:51:27" ...

``` r
d <- rep(base::date(), 1000000)

timings <- microbenchmark::microbenchmark(f1(d),
                                          f2(d))
print(timings)
```

    ## Unit: milliseconds
    ##   expr       min        lq     mean    median        uq       max neval
    ##  f1(d) 1788.3708 1845.0248 1906.850 1870.2964 1935.0941 2396.5818   100
    ##  f2(d)  200.1014  217.7617  245.528  233.5837  276.0612  332.2283   100
    ##  cld
    ##    b
    ##   a
