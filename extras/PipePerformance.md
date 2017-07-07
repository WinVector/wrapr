Some timings for [`%.>%`](http://www.win-vector.com/blog/2017/07/in-praise-of-syntactic-sugar/).

Keep in mind for any *serious* application the calculation time on data will far dominate any piping overhead, but it is fun to look.

So we will compare:

-   `magrittr*` `magrittr::%>%` substitution.
-   `BlockPipe*` `wrapr::%.>%` substitution.
-   `BizarroPipe*` `%->%` substitution.

``` r
library("microbenchmark")
library("wrapr")
library("rlang")
suppressPackageStartupMessages(library("ggplot2"))
suppressPackageStartupMessages(library("dplyr"))

# load generated examples
source("pGenFns.R")
```

``` r
bm <- microbenchmark(
  magrittr_1(),
  BlockPipe_1(),
  BizarroPipe_1(),
  magrittr_2(),
  BlockPipe_2(),
  BizarroPipe_2(),
  magrittr_5(),
  BlockPipe_5(),
  BizarroPipe_5(),
  magrittr_10(),
  BlockPipe_10(),
  BizarroPipe_10(),
  magrittr_15(),
  BlockPipe_15(),
  BizarroPipe_15(),
  magrittr_20(),
  BlockPipe_20(),
  BizarroPipe_20(),
  magrittr_25(),
  BlockPipe_25(),
  BizarroPipe_25(), 
  magrittr_50(),
  BlockPipe_50(),
  BizarroPipe_50(), 
  times=1000L
)
print(bm)
```

    ## Unit: nanoseconds
    ##              expr     min        lq        mean    median        uq
    ##      magrittr_1()   49781   59255.5   85051.347   66359.0   90181.0
    ##     BlockPipe_1()   36175   42627.5   61577.847   49406.5   59746.0
    ##   BizarroPipe_1()     235    1211.0    2842.215    1558.5    1873.0
    ##      magrittr_2()   71440   84197.0  117532.574   92586.0  118851.5
    ##     BlockPipe_2()   72697   82110.0  111571.442   90862.5  102834.5
    ##   BizarroPipe_2()     317    1327.0    3384.249    1755.0    2121.5
    ##      magrittr_5()  135365  152185.0  199512.440  161659.5  190634.5
    ##     BlockPipe_5()  183170  199254.0  261161.787  211771.0  246574.5
    ##   BizarroPipe_5()     414    1652.5    5059.542    2044.0    2438.5
    ##     magrittr_10()  234602  266156.0  341311.489  281296.0  322047.5
    ##    BlockPipe_10()  366957  395526.5  497202.059  413906.5  469208.0
    ##  BizarroPipe_10()     593    1878.5    7775.206    2274.0    2721.0
    ##     magrittr_15()  339428  376478.0  481947.856  398323.5  450048.0
    ##    BlockPipe_15()  553091  595076.0  745554.158  621513.0  718897.5
    ##  BizarroPipe_15()     763    2302.0   10656.185    2737.0    3123.0
    ##     magrittr_20()  442190  491065.5  619330.004  516669.5  573511.5
    ##    BlockPipe_20()  736576  792184.0 1034784.120  823402.5  950290.0
    ##  BizarroPipe_20()     919    2231.5   12584.827    2617.0    3063.0
    ##     magrittr_25()  542720  600915.5  759384.570  640097.0  729840.0
    ##    BlockPipe_25()  922675  995450.5 1239293.559 1036901.0 1225043.5
    ##  BizarroPipe_25()    1081    2304.0   15578.697    2704.0    3099.5
    ##     magrittr_50() 1060529 1178074.5 1481767.953 1254642.5 1468602.5
    ##    BlockPipe_50() 1907716 2045163.5 2579499.864 2144565.0 2503644.5
    ##  BizarroPipe_50()    1921    3377.0   29146.116    3830.0    4331.0
    ##       max neval
    ##   1526461  1000
    ##   1457298  1000
    ##   1232725  1000
    ##   2142285  1000
    ##   2111582  1000
    ##   1633434  1000
    ##   7291408  1000
    ##   6110002  1000
    ##   2953704  1000
    ##   6833855  1000
    ##  10310515  1000
    ##   5442512  1000
    ##  12457795  1000
    ##  13130020  1000
    ##   7870331  1000
    ##  13894025  1000
    ##  57802202  1000
    ##   9868714  1000
    ##  18721051  1000
    ##  17962070  1000
    ##  12813065  1000
    ##  35129574  1000
    ##  54344076  1000
    ##  25203576  1000

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
mkPlot(d[d$fn %in% c('magrittr', 'BlockPipe'), , drop=FALSE], 
       'magrittr v.s. BlockPipe')
```

![](PipePerformance_files/figure-markdown_github/replot-2.png)

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

    ##   Intercept       size          fn
    ## 1  2332.175   534.1378 BizarroPipe
    ## 2 -3831.490 51260.1309   BlockPipe
    ## 3 56177.712 28409.5042    magrittr

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

crossingPoint <- solve(dfits, 'BlockPipe', 'magrittr')
print(crossingPoint)
```

    ## [1] 2.626151

``` r
ratio <- dfits$size[dfits$fn=='BlockPipe'] / dfits$size[dfits$fn=='magrittr']
print(ratio)
```

    ## [1] 1.80433

Overall:

BlockPipe is about 1.8 times slower than magrittr on large pipelines, though this cost will be unnoticeable with in any significant workload.
