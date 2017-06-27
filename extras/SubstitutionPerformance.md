Some timings for [`wrapr::let()`](https://github.com/WinVector/wrapr).

Keep in mind for any *serious* application the calculation time on data will far dominate any expression re-write time from either `rlang`/`tidyeval` or `wrapr`. But it has been [asked what the timings are](http://www.win-vector.com/blog/2017/06/please-consider-using-wraprlet-for-replacement-tasks/#comment-66574), and it is fun to look.

So we will compare:

-   `fWrapr*` `wrapr::let()` substitution (`"langsubs"` mode).
-   `fTidyN*` `rlang::eval_tidy()` substitution to quoted names (the closest I found to `wrapr::let()`).
-   `fTidyQ*` `rlang::eval_tidy()` substitution to `quo()` free names (what seems to be the preferred case/notation by `rlang` developers as it moves from NSE (non-standard evaluation interface) to NSE).

``` r
library("microbenchmark")
library("wrapr")
library("rlang")
suppressPackageStartupMessages(library("ggplot2"))
suppressPackageStartupMessages(library("dplyr"))

# load generated examples
source("genFns.R")

# load up vars
nvars <- 200
for(i in seq(0, nvars-1)) {
  assign(paste('var', i, sep='_'), i)
}

fWrapr_1 <- function() {
  let(
   c( NM_0 = 'var_0' ),
   NM_0
 )}
 
fTidyN_1 <- function() {
   NM_0 = as.name('var_0')
   eval_tidy(quo( (!!NM_0) ))
}
 
fTidyQ_1 <- function() {
   NM_0 = quo(var_0)
   eval_tidy(quo( (!!NM_0) ))
}

fWrapr_1()
```

    ## [1] 0

``` r
fTidyN_1()
```

    ## [1] 0

``` r
fTidyQ_1()
```

    ## [1] 0

``` r
fWrapr_5 <- function() {
  let(
   c( NM_0 = 'var_0', NM_1 = 'var_1', NM_2 = 'var_2', NM_3 = 'var_3', NM_4 = 'var_4' ),
   NM_0 + NM_1 + NM_2 + NM_3 + NM_4
 )}
 
fTidyN_5 <- function() {
   NM_0 = as.name('var_0')
   NM_1 = as.name('var_1')
   NM_2 = as.name('var_2')
   NM_3 = as.name('var_3')
   NM_4 = as.name('var_4')
   eval_tidy(quo( (!!NM_0) + (!!NM_1) + (!!NM_2) + (!!NM_3) + (!!NM_4) ))
}
 
fTidyQ_5 <- function() {
   NM_0 = quo(var_0)
   NM_1 = quo(var_1)
   NM_2 = quo(var_2)
   NM_3 = quo(var_3)
   NM_4 = quo(var_4)
   eval_tidy(quo( (!!NM_0) + (!!NM_1) + (!!NM_2) + (!!NM_3) + (!!NM_4) ))
}


fWrapr_5()
```

    ## [1] 10

``` r
fTidyN_5()
```

    ## [1] 10

``` r
fTidyQ_5()
```

    ## [1] 10

``` r
fWrapr_25()
```

    ## [1] 300

``` r
fTidyN_25()
```

    ## [1] 300

``` r
fTidyQ_25()
```

    ## [1] 300

``` r
bm <- microbenchmark(
  fWrapr_1(),
  fTidyN_1(),
  fTidyQ_1(),
  fWrapr_5(),
  fTidyN_5(),
  fTidyQ_5(),
  fWrapr_10(),
  fTidyN_10(),
  fTidyQ_10(),
  fWrapr_25(),
  fTidyN_25(),
  fTidyQ_25(), 
  times=1000L
)
print(bm)
```

    ## Unit: microseconds
    ##         expr       min         lq       mean    median         uq
    ##   fWrapr_1()   145.735   181.1520   237.9062   220.708   251.5510
    ##   fTidyN_1()  1365.729  1446.4355  1792.4694  1608.348  1970.5275
    ##   fTidyQ_1()  1938.561  2073.4885  2579.4358  2302.308  2886.3375
    ##   fWrapr_5()   330.086   393.6680   490.4902   428.039   512.1080
    ##   fTidyN_5()  1386.237  1474.9000  1884.3215  1657.920  2090.2950
    ##   fTidyQ_5()  4288.974  4620.4470  5603.4359  5225.815  6333.5840
    ##  fWrapr_10()   556.672   622.0985   773.5402   664.699   818.9505
    ##  fTidyN_10()  1400.824  1494.5610  1894.3997  1655.782  2050.6070
    ##  fTidyQ_10()  7208.277  7750.0125  9437.7881  8948.324 10409.2875
    ##  fWrapr_25()  1260.949  1355.7435  1616.4876  1443.938  1740.2485
    ##  fTidyN_25()  1471.529  1566.0075  1949.3996  1740.274  2187.2590
    ##  fTidyQ_25() 16016.757 17964.6150 20568.9579 20125.162 22293.2890
    ##        max neval
    ##   3402.335  1000
    ##   7826.433  1000
    ##  11562.311  1000
    ##   5733.013  1000
    ##  18709.916  1000
    ##  24059.602  1000
    ##  11497.049  1000
    ##  44358.866  1000
    ##  71817.655  1000
    ##   4329.779  1000
    ##   5241.169  1000
    ##  85265.523  1000

``` r
autoplot(bm)
```

![](SubstitutionPerformance_files/figure-markdown_github/timings-1.png)

``` r
d <- as.data.frame(bm)
d$size <- as.numeric(gsub("[^0-9]+", "", d$expr))
d$fn <- gsub("[_0-9].*$", "", d$expr)

mkPlot <- function(d, title) {
  d$size <- as.factor(d$size)
  highCut <- as.numeric(quantile(d$time, probs = 0.99))
  dcut <- d[d$time<=highCut, , drop=FALSE]
  
  ggplot(data=dcut, aes(x=time, group=expr, color=size)) +
    geom_density(adjust=0.3) +
    facet_wrap(~fn, ncol=1, scales = 'free_y') +
    xlab('time (NS)') + ggtitle(title)
}

mkPlot(d, 'all timings')
```

![](SubstitutionPerformance_files/figure-markdown_github/replot-1.png)

``` r
mkPlot(d[d$fn %in% c('fWrapr', 'fTidyN'), , drop=FALSE], 
       'fWrapr v.s. fTidyN')
```

![](SubstitutionPerformance_files/figure-markdown_github/replot-2.png)

``` r
mkPlot(d[d$fn %in% c('fTidyQ', 'fTidyN'), , drop=FALSE], 
       'fTidyQ v.s. fTidyN')
```

![](SubstitutionPerformance_files/figure-markdown_github/replot-3.png)

``` r
# fit a linear function for runtime as a function of size
# per group.
fits <- d %>%
  split(., .$fn) %>%
  lapply(., 
         function(di) { 
           lm(time ~ size, data=di) 
         }) %>%
  lapply(., coefficients) %>%
  lapply(., 
         function(ri) {
           data.frame(Intercept= ri[["(Intercept)"]],
                      size= ri[['size']])
         }) 
dfits <- bind_rows(fits)
dfits$fn <- names(fits)

# "Intercept" is roughly start-up cost 
# "size" is slope or growth rate
print(dfits)
```

    ##   Intercept       size     fn
    ## 1 1824147.9   5463.382 fTidyN
    ## 2 1869438.2 749069.878 fTidyQ
    ## 3  194694.4  57064.556 fWrapr

``` r
# solve for size where two lines interesect.
# Note: this is a naive estimate, and not stable
# in the face of estimated slopes and intercepts.
solve <- function(dfits, f1, f2) {
  idx1 <- which(dfits$fn==f1)
  idx2 <- which(dfits$fn==f2)
  size <- (dfits$Intercept[[idx1]] - dfits$Intercept[[idx2]]) /
    (dfits$size[[idx2]] - dfits$size[[idx1]])
  size
}

crossingPoint <- solve(dfits, 'fTidyN', 'fWrapr')
print(crossingPoint)
```

    ## [1] 31.57784

Overall:

-   Remember: these timings are *not* important, for any interesting calculation data manipulation time will quickly dominate expression manipulation time (meaning [tuning here is not important](https://en.wikipedia.org/wiki/Amdahl%27s_law)).
-   `fWrapr*` is fastest, but seems to have worse size dependent growth rate (or slope) than `fTidyN*`. This means that we would expect at some large substitution size `fTidyN*` could become quicker (about 32 or more variables). Likely `wrapr::let()` is paying too much for a map-lookup somewhere and this could be fixed at some point.
-   `fTidyQ*` is very much slower with a much worse slope. Likely the slope is also some expensive mapping that can also be fixed.
