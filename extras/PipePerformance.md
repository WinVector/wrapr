Some timings for [`%.>%`](http://www.win-vector.com/blog/2017/07/in-praise-of-syntactic-sugar/).

Keep in mind for any *serious* application the calculation time on data will far dominate any piping overhead, but it is fun to look.

So we will compare:

-   `magrittr*` `magrittr::%>%` substitution.
-   `DotBlockPipe*` `wrapr::%.>%` substitution.
-   `BizarroPipe*` `->.;` substitution.

``` r
library("microbenchmark")
library("wrapr")
library("rlang")
suppressPackageStartupMessages(library("ggplot2"))
suppressPackageStartupMessages(library("dplyr"))

# load generated examples
source("pGenFns.R")

print(BizarroPipe_5)
```

    ## function () 
    ## {
    ##     . <- 5
    ##     . <- sin(.)
    ##     . <- sin(.)
    ##     . <- sin(.)
    ##     . <- sin(.)
    ##     sin(.)
    ## }

``` r
print(DotBlockPipe_5)
```

    ## function () 
    ## {
    ##     5 %.>% sin(.) %.>% sin(.) %.>% sin(.) %.>% sin(.) %.>% sin(.)
    ## }

``` r
print(magrittr_5)
```

    ## function () 
    ## {
    ##     5 %>% sin(.) %>% sin(.) %>% sin(.) %>% sin(.) %>% sin(.)
    ## }

``` r
BizarroPipe_10()
```

    ## [1] -0.4774053

``` r
DotBlockPipe_10()
```

    ## [1] -0.4774053

``` r
magrittr_10()
```

    ## [1] -0.4774053

``` r
bm <- microbenchmark(
  magrittr_1(),
  DotBlockPipe_1(),
  BizarroPipe_1(),
  magrittr_2(),
  DotBlockPipe_2(),
  BizarroPipe_2(),
  magrittr_5(),
  DotBlockPipe_5(),
  BizarroPipe_5(),
  magrittr_10(),
  DotBlockPipe_10(),
  BizarroPipe_10(),
  magrittr_15(),
  DotBlockPipe_15(),
  BizarroPipe_15(),
  magrittr_20(),
  DotBlockPipe_20(),
  BizarroPipe_20(),
  magrittr_25(),
  DotBlockPipe_25(),
  BizarroPipe_25(), 
  magrittr_50(),
  DotBlockPipe_50(),
  BizarroPipe_50(), 
  times=1000L
)
print(bm)
```

    ## Unit: nanoseconds
    ##               expr     min        lq        mean    median        uq
    ##       magrittr_1()   49785   56912.5   69493.461   61248.0   68033.5
    ##   DotBlockPipe_1()   35843   40754.5   49773.803   45063.0   51652.5
    ##    BizarroPipe_1()     235    1007.0    2574.675    1483.5    1738.0
    ##       magrittr_2()   72442   81543.5   94173.487   86616.0   93575.0
    ##   DotBlockPipe_2()   71109   79621.0   97903.294   87159.5   95066.5
    ##    BizarroPipe_2()     283    1224.5    3460.313    1596.5    1815.5
    ##       magrittr_5()  134732  150432.0  169389.760  156668.5  166102.0
    ##   DotBlockPipe_5()  178816  195179.0  224559.392  204326.5  217448.0
    ##    BizarroPipe_5()     395    1409.5    4951.969    1859.0    2124.5
    ##      magrittr_10()  236226  259765.5  295810.353  271615.5  284856.5
    ##  DotBlockPipe_10()  359045  387702.0  430698.131  400453.5  417779.0
    ##   BizarroPipe_10()     569    1690.5    7909.038    2161.0    2454.5
    ##      magrittr_15()  336208  370699.5  423379.597  386789.0  403934.0
    ##  DotBlockPipe_15()  541747  581219.5  639857.959  596953.5  616747.5
    ##   BizarroPipe_15()     725    1643.5    9429.208    2185.0    2486.5
    ##      magrittr_20()  438164  484500.0  547503.970  506089.5  526963.0
    ##  DotBlockPipe_20()  726609  773966.5  869269.764  794647.5  824726.5
    ##   BizarroPipe_20()     890    2054.5   12136.100    2523.0    2842.5
    ##      magrittr_25()  539184  595130.5  677442.105  623097.5  648832.5
    ##  DotBlockPipe_25()  908188  972307.5 1068904.649  998401.0 1033680.0
    ##   BizarroPipe_25()    1055    2129.5   15094.721    2493.5    2799.0
    ##      magrittr_50() 1060972 1146916.0 1362917.546 1205028.0 1251479.5
    ##  DotBlockPipe_50() 1880236 1993322.0 2194177.990 2045266.5 2139704.0
    ##   BizarroPipe_50()    1883    2945.5   27738.108    3542.5    3899.5
    ##       max neval
    ##   1438948  1000
    ##   1412537  1000
    ##   1197516  1000
    ##   2014716  1000
    ##   2201503  1000
    ##   1945315  1000
    ##   3936573  1000
    ##   3907841  1000
    ##   3211876  1000
    ##   7841107  1000
    ##   9174931  1000
    ##   5822802  1000
    ##  10457276  1000
    ##  12526355  1000
    ##   7360532  1000
    ##  15202177  1000
    ##  14699481  1000
    ##   9688122  1000
    ##  17094708  1000
    ##  20796798  1000
    ##  12602971  1000
    ##  54939886  1000
    ##  35825099  1000
    ##  24298838  1000

``` r
autoplot(bm)
```

![](PipePerformance_files/figure-markdown_github/timings-1.png)

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

![](PipePerformance_files/figure-markdown_github/replot-1.png)

``` r
mkPlot(d[d$fn %in% c('magrittr', 'DotBlockPipe'), , drop=FALSE], 
       'magrittr v.s. DotBlockPipe')
```

![](PipePerformance_files/figure-markdown_github/replot-2.png)

``` r
# fit a linear function for runtime as a function of size
# per group.
fits <- d %.>%
  split(., .$fn) %.>%
  lapply(., 
         function(di) { 
           lm(time ~ size, data=di) 
         }) %.>%
  lapply(., coefficients) %.>%
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

    ##    Intercept       size           fn
    ## 1  2297.5922   507.1359  BizarroPipe
    ## 2  -817.0898 43606.8883 DotBlockPipe
    ## 3 34019.6226 26312.1351     magrittr

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

crossingPoint <- solve(dfits, 'DotBlockPipe', 'magrittr')
print(crossingPoint)
```

    ## [1] 2.014294

``` r
ratio <- dfits$size[dfits$fn=='DotBlockPipe'] / dfits$size[dfits$fn=='magrittr']
print(ratio)
```

    ## [1] 1.657292

Overall:

DotBlockPipe is about 1.7 times slower than magrittr on large pipelines, though this cost will be unnoticeable with in any significant workload.
