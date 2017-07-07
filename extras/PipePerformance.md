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
    ##      magrittr_1()   48358   56814.5   72349.506   61707.0   69379.5
    ##     BlockPipe_1()   36342   41260.0   56146.992   46720.5   53388.5
    ##   BizarroPipe_1()     228    1151.5    2782.532    1502.0    1746.5
    ##      magrittr_2()   71794   81523.0  101974.503   87467.0   97218.0
    ##     BlockPipe_2()   72715   80186.0  101620.186   87832.5   97135.5
    ##   BizarroPipe_2()     285    1219.0    3968.133    1683.5    1954.0
    ##      magrittr_5()  133230  150451.0  178406.139  158425.0  172480.0
    ##     BlockPipe_5()  180786  196797.5  230460.008  206686.5  219939.0
    ##   BizarroPipe_5()     403    1521.0    5205.534    1947.0    2217.0
    ##     magrittr_10()  228767  257994.5  300887.610  269757.0  287901.0
    ##    BlockPipe_10()  363061  390542.0  454202.538  403860.0  426440.0
    ##  BizarroPipe_10()     563    1750.5    8387.110    2201.0    2488.5
    ##     magrittr_15()  332266  365863.5  431704.700  385408.5  409156.0
    ##    BlockPipe_15()  542296  585688.5  662728.176  602371.5  636772.5
    ##  BizarroPipe_15()     723    1693.5   11088.090    2235.0    2567.0
    ##     magrittr_20()  430662  475124.5  567997.874  502081.0  531365.5
    ##    BlockPipe_20()  724692  780434.0  931520.994  801213.0  840898.5
    ##  BizarroPipe_20()     888    2220.0   14462.785    2648.5    2968.5
    ##     magrittr_25()  536797  588008.5  754831.838  620143.5  651944.5
    ##    BlockPipe_25()  912201  978554.0 1103072.255 1005178.5 1066836.0
    ##  BizarroPipe_25()    1059    2115.0   28047.781    2592.5    2928.0
    ##     magrittr_50() 1042338 1129140.5 1337114.485 1192483.0 1266367.5
    ##    BlockPipe_50() 1893602 2015447.5 2274265.264 2066250.5 2212909.5
    ##  BizarroPipe_50()    1879    3180.0   44393.499    3676.0    4080.5
    ##       max neval
    ##   1628357  1000
    ##   2025437  1000
    ##   1356623  1000
    ##   3593319  1000
    ##   3053381  1000
    ##   2368036  1000
    ##   4441564  1000
    ##   4904597  1000
    ##   3311099  1000
    ##   8014501  1000
    ##  15922508  1000
    ##   6281205  1000
    ##  12333099  1000
    ##  12920280  1000
    ##   8917189  1000
    ##  24491607  1000
    ##  52400881  1000
    ##  11888527  1000
    ##  50495117  1000
    ##  26841575  1000
    ##  25494753  1000
    ##  50034871  1000
    ##  48635167  1000
    ##  40718551  1000

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

    ##    Intercept       size          fn
    ## 1   785.8486   875.3803 BizarroPipe
    ## 2  3687.7523 45191.5187   BlockPipe
    ## 3 50560.1921 26099.8837    magrittr

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

    ## [1] 2.45513

``` r
ratio <- dfits$size[dfits$fn=='BlockPipe'] / dfits$size[dfits$fn=='magrittr']
print(ratio)
```

    ## [1] 1.731484

Overall:

BlockPipe is about 2 times slower than magrittr on large pipelines.
