VectorizeM Example
================

``` r
date_time <- "2018-10-12 16:12:28 PDT"
d <- rep(date_time, 3)

f1 <- function(d) {
  lubridate::ymd_hms(d)
}

str(f1(d))
```

    ##  POSIXct[1:3], format: "2018-10-12 16:12:28" "2018-10-12 16:12:28" ...

``` r
f2 <- wrapr::VectorizeM(f1, 
                        SIMPLIFY = FALSE, 
                        UNLIST = TRUE)

str(f2(d))
```

    ##  POSIXct[1:3], format: "2018-10-12 16:12:28" "2018-10-12 16:12:28" ...

``` r
# generate lots of date strings, but only from a few 
# dates
d <- rep(f1(date_time), 1000000)
d <- d + sample.int(100, size = length(d), replace = TRUE)
d <- as.character(d)

timings <- microbenchmark::microbenchmark(f1(d),
                                          f2(d))
print(timings)
```

    ## Unit: milliseconds
    ##   expr      min       lq     mean   median       uq       max neval cld
    ##  f1(d) 164.3644 204.9510 238.6316 226.5286 271.1638  362.0206   100  a 
    ##  f2(d) 445.0025 475.5262 523.4344 492.2326 540.6395 1002.9361   100   b
